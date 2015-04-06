(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

module L    = Log
module Log  = Log.Make (struct let section = "[Client]" end)
module ARC4 = Nocrypto.Cipher_stream.ARC4
module Cs   = Nocrypto.Uncommon.Cs

open Lwt.Infix

let listen_backlog = 5
let listen_ports = [50000]
let choke_timeout = 5.
let rechoke_interval = 10.
let rechoke_optimistic_duration = 2
let rechoke_slots = 10
let block_size = 1 lsl 14 (* 16384 *)
let max_requests = 5

type addr = Unix.inet_addr * int

type event =
  | PeersReceived of addr list
  | PeerConnected of addr * Lwt_unix.file_descr * bool (* bool = incoming *)
  | PieceVerified of int
  | PieceFailed of int
  | HandshakeFailed of addr
  | HandshakeOk of addr * (ARC4.key * ARC4.key) option * Lwt_unix.file_descr * Bits.t * SHA1.t
  | TorrentComplete
  | PeerEvent of SHA1.t * Peer.event

let log_event = function
  | PeersReceived peers ->
      Log.debug "+ PEERS RECEIVED %d" (List.length peers)
  | HandshakeOk (_, _, _, _, id) ->
      Log.info "+ HANDSHAKE SUCCESS id:%s" (SHA1.to_hex_short id)
  | PeerConnected ((ip, p), _, incoming) ->
      Log.info "+ CONNECT SUCCESS %s addr:%s port:%d" (if incoming then "INCOMING" else "OUTGOING")
        (Unix.string_of_inet_addr ip) p
  | PieceVerified i ->
      Log.info "+ PIECE VERIFIED idx:%d" i
  | PieceFailed i ->
      Log.info "+ PIECE FAILED idx:%d" i
  | HandshakeFailed (ip, p) ->
      Log.info "+ HANDSHAKE FAIL addr:%s port:%d" (Unix.string_of_inet_addr ip) p
  | PeerEvent (id, _) ->
      ()
  | TorrentComplete ->
      Log.debug "+ TORRENT COMPLETE"

type t =
  { id : SHA1.t;
    ih : SHA1.t;
    trackers : Uri.t list;
    peer_mgr : PeerMgr.swarm;
    chan : event Lwt_stream.t;
    push : event -> unit }

(* let pex_delay = 60.0 *)

(* let rec pex_pulse pm = *)
(*   let pex = Hashtbl.fold (fun addr _ l -> addr :: l) pm.peers [] in *)
(*   iter_peers (fun p -> Peer.send_pex p pex) pm; *)
(*   Lwt_unix.sleep pex_delay >>= fun () -> pex_pulse pm *)

let connect_to_peer ~id ~info_hash addr fd push =
  let push = function
    | Handshake.Ok (mode, ext, peer_id) ->
        push (HandshakeOk (addr, mode, fd, ext, peer_id))
    | Handshake.Failed ->
        push (HandshakeFailed addr)
  in
  Handshake.outgoing ~id ~info_hash addr fd push

let welcome push mode fd exts id =
  Peer.create id (fun e -> push (PeerEvent (id, e))) fd mode
  (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)

let rechoke_compare (p1, salt1) (p2, salt2) =
  if Peer.download_speed p1 <> Peer.download_speed p2 then
    compare (Peer.download_speed p2) (Peer.download_speed p1)
  else
  if Peer.upload_speed p1 <> Peer.upload_speed p2 then
    compare (Peer.upload_speed p2) (Peer.upload_speed p1)
  else
  if Peer.am_choking p1 <> Peer.am_choking p2 then
    compare (Peer.am_choking p1) (Peer.am_choking p2)
  else
    compare salt1 salt2

let rechoke peers =
  let rec loop opt nopt =
    Log.info "RECHOKING";
    let opt, nopt = if nopt > 0 then opt, nopt - 1 else None, nopt in
    let wires =
      let add _ p wires =
        match (* Peer.is_seeder p, *)false, opt with (* FIXME FIXME *)
        | true, _ ->
            Peer.choke p;
            wires
        | false, Some opt when SHA1.equal (Peer.id p) opt ->
            wires
        | false, _ ->
            (p, Random.int (1 lsl 29)) :: wires
      in
      Hashtbl.fold add peers []
    in
    let wires = List.sort rechoke_compare wires in
    let rec select n acc = function
      | (p, _) as w :: wires when n < rechoke_slots ->
          let n = if Peer.peer_interested p then n + 1 else n in
          select n (w :: acc) wires
      | wires ->
          begin match opt with
          | Some _ ->
              acc, wires, opt, nopt
          | None ->
              let wires = List.filter (fun (p, _) -> Peer.peer_interested p) wires in
              if List.length wires > 0 then
                let (p, _) as opt = List.nth wires (Random.int (List.length wires)) in
                (opt :: acc), wires, Some (Peer.id p), rechoke_optimistic_duration
              else
                acc, wires, None, nopt
          end
    in
    let unchoke, choke, opt, nopt = select 0 [] wires in
    List.iter (fun (p, _) -> Peer.unchoke p) unchoke;
    List.iter (fun (p, _) -> Peer.choke p) choke;
    Lwt_unix.sleep choke_timeout >>= fun () -> loop opt nopt
  in
  loop None rechoke_optimistic_duration

type piece_state =
  | Pending
  | Active of int array
  | Verified

type piece =
  { mutable state : piece_state;
    length : int;
    offset : int64;
    mutable have : int;
    hash : SHA1.t }

let request_block parts = function
  | false ->
      let rec loop i =
        if i >= Array.length parts then
          None
        else
        if parts.(i) = 0 then
          (parts.(i) <- 1; Some i)
        else
          loop (i + 1)
      in
      loop 0
  | true ->
      let rec loop min index i =
        if i >= Array.length parts then
          if index >= 0 then
            (parts.(index) <- parts.(index) + 1; Some index)
          else
            None
        else
        if parts.(i) >= 0 && (parts.(i) < min || min < 0) then
          loop parts.(i) i (i + 1)
        else
          loop min index (i + 1)
      in
      loop (-1) (-1) 0

let request_endgame pieces has =
  let rec loop i =
    if i >= Array.length pieces then
      None
    else
      match pieces.(i).state with
      | Verified
      | Pending ->
          loop (i + 1)
      | Active parts ->
          if has i then
            match request_block parts true with
            | Some j ->
                Some (i, j)
            | None ->
                None
          else
            loop (i + 1)
  in
  loop 0

let request_pending pieces has start finish =
  if start < 0 || start > finish || finish > Array.length pieces then
    invalid_arg "request_pending";
  let rec loop i =
    if i >= finish then
      None
    else
      match pieces.(i).state with
      | Pending ->
          let nparts = (pieces.(i).length + block_size - 1) / block_size in
          let parts = Array.make nparts 0 in
          pieces.(i).state <- Active parts;
          begin match request_block parts false with
          | None ->
              assert false
          | Some j ->
              Some (i, j)
          end
      | Active _
      | Verified ->
          loop (i + 1)
  in
  loop start

let request_pending pieces has =
  let n = Random.int (Array.length pieces + 1) in
  match request_pending pieces has n (Array.length pieces) with
  | None ->
      begin match request_pending pieces has 0 n with
      | None ->
          request_endgame pieces has
      | Some _ as r ->
          r
      end
  | Some _ as r ->
      r

let request_active pieces has =
  let rec loop i =
    if i >= Array.length pieces then
      request_pending pieces has
    else
      match pieces.(i).state with
      | Verified
      | Pending ->
          loop (i + 1)
      | Active parts ->
          if has i then
            match request_block parts false with
            | None ->
                loop (i + 1)
            | Some j ->
                Some (i, j)
          else
            loop (i + 1)
  in
  loop 0

let request p pieces =
  if Peer.requests p < max_requests then
    match request_active pieces (Peer.has p) with
    | Some (i, j) ->
        let off = j * block_size in
        let len = pieces.(i).length mod block_size in
        let len = if len = 0 then block_size else len in
        Peer.request p i off len
    | None ->
        ()

let update_requests pieces peers =
  Hashtbl.iter (fun _ p -> if not (Peer.peer_choking p) then request p pieces) peers

let update_interest pieces p =
  let rec loop i =
    if i < Array.length pieces then
      if Peer.has p i then
        match pieces.(i).state with
        | Active _
        | Pending ->
            Peer.interested p
        | Verified ->
            loop (i + 1)
      else
        loop (i + 1)
    else
      Peer.not_interested p
  in
  loop 0

(* let update_interest pieces peers = *)
(*   Hashtbl.iter (fun _ p -> update_interest pieces p) peers *)

let send_block store pieces p i off len =
  match pieces.(i).state with
  | Verified ->
      let off1 = Int64.(add pieces.(i).offset (of_int off)) in
      Lwt.ignore_result (Store.read store off1 len >>= Lwt.wrap4 Peer.piece p i off)
  | Pending
  | Active _ ->
      ()

let verify_piece store peers pieces i push =
  Store.digest store pieces.(i).offset pieces.(i).length >>= fun sha ->
  if SHA1.equal sha pieces.(i).hash then
    Lwt.wrap1 push @@ PieceVerified i
  else
    Lwt.wrap1 push @@ PieceFailed i

let record_block store peers pieces i off s push =
  match pieces.(i).state with
  | Pending
  | Verified ->
      Lwt.return_unit
  | Active parts ->
      let j = off / block_size in
      let c = parts.(j) in
      if c >= 0 then begin
        parts.(j) <- (-1);
        let rec cancel _ p =
          if Peer.requested p i j (Cstruct.len s) then
            Peer.cancel p i j (Cstruct.len s)
        in
        if c > 1 then Hashtbl.iter cancel peers;
        pieces.(i).have <- pieces.(i).have + 1
      end;
      let off = Int64.(add pieces.(i).offset (of_int off)) in
      Store.write store off s >>= fun () ->
      if pieces.(i).have = Array.length parts then
        verify_piece store peers pieces i push
      else
        Lwt.return_unit

let request_rejected pieces (i, off, _) =
  match pieces.(i).state with
  | Verified
  | Pending -> ()
  | Active parts ->
      let j = off / block_size in
      if parts.(j) > 0 then parts.(j) <- parts.(j) - 1

let record_block store peers pieces i off s push =
  Lwt.ignore_result (record_block store peers pieces i off s push)

let share_torrent bt meta store pieces have peers =
  Log.info "TORRENT SHARE have:%d total:%d peers:%d"
    (Bits.count_ones have) (Bits.length have) (Hashtbl.length peers);
  let peer id f = try let p = Hashtbl.find peers id in f p with Not_found -> () in
  Hashtbl.iter (fun _ p -> Peer.have_bitfield p have; update_interest pieces p) peers;
  update_requests pieces peers;
  Lwt.ignore_result (rechoke peers);
  let rec loop () =
    Lwt_stream.next bt.chan >>= fun e ->
    log_event e;
    match e with
    | TorrentComplete ->
        (* FIXME TODO FIXME *)
        Log.info "TORRENT COMPLETE";
        Lwt.return_unit

    | PeersReceived addrs ->
        List.iter (PeerMgr.add bt.peer_mgr) addrs;
        loop ()

    | PieceVerified i ->
        pieces.(i).state <- Verified;
        Bits.set have i;
        Hashtbl.iter (fun _ p -> Peer.have p i) peers;
        (* maybe update interest ? *)
        loop ()

    | PieceFailed i ->
        (* FIXME FIXME *)
        loop ()

    | HandshakeFailed addr ->
        PeerMgr.handshake_failed bt.peer_mgr addr;
        loop ()

    | PeerEvent (id, Peer.PeerDisconnected reqs) ->
        List.iter (request_rejected pieces) reqs;
        PeerMgr.peer_disconnected bt.peer_mgr id;
        Hashtbl.remove peers id;
        (* if not (am_choking peers id) && peer_interested peers id then Choker.rechoke ch; *)
        loop ()

    | PeerEvent (_, Peer.Choked reqs) ->
        List.iter (request_rejected pieces) reqs;
        loop ()

    | PeerEvent (id, Peer.Unchoked)
    | PeerEvent (id, Peer.Have _)
    | PeerEvent (id, Peer.HaveBitfield _) ->
        peer id (update_interest pieces);
        update_requests pieces peers;
        loop ()

    | PeerEvent (_, Peer.Interested)
    | PeerEvent (_, Peer.NotInterested) ->
        (* rechoke *)
        loop ()

    | PeerEvent (id, Peer.MetaRequested i) ->
        peer id
          (Peer.metadata_piece (Metadata.length meta) i (Metadata.get_piece meta i));
        loop ()

    | PeerEvent (_, Peer.GotMetaPiece _)
    | PeerEvent (_, Peer.RejectMetaPiece _) ->
        loop ()

    | PeerEvent (id, Peer.BlockRequested (i, off, len)) ->
        if i >= 0 && i < Array.length pieces then
          peer id (fun p -> send_block store pieces p i off len)
        else
          Log.warn "! PEER REQUEST invalid piece:%d" i;
        loop ()

    | PeerEvent (_, Peer.BlockReceived (i, off, s)) ->
        if i >= 0 && i < Array.length pieces then
          record_block store peers pieces i off s bt.push
        else
          Log.warn "! PIECE invalid piece:%d" i;
        loop ()

    | PeerEvent (_, Peer.GotPEX (added, dropped)) ->
        List.iter (fun (addr, _) -> PeerMgr.add bt.peer_mgr addr) added;
        loop ()

    | PeerEvent (_, Peer.DHTPort _) ->
        (* let addr, _ = Peer.addr p in *)
        (* Lwt.async begin fun () -> *)
        (*   DHT.ping bt.dht (addr, i) >|= function *)
        (*   | Some (id, addr) -> *)
        (*       DHT.update bt.dht Kademlia.Good id addr *)
        (*   | None -> *)
        (*       debug "%s did not reply to dht ping on port %d" (Peer.to_string p) i *)
        (* end; *)
        (* FIXME *)
        loop ()

    | PeerConnected (addr, fd, false) ->
        connect_to_peer ~id:bt.id ~info_hash:bt.ih addr fd bt.push;
        loop ()

    | PeerConnected (_, _, true) ->
        (* FIXME FIXME *)
        loop ()

    | HandshakeOk (addr, mode, sock, exts, id) ->
        PeerMgr.handshake_ok bt.peer_mgr addr id;
        let p = welcome bt.push mode sock exts id in
        Peer.have_bitfield p have;
        Hashtbl.replace peers id p;
        update_interest pieces p;
        update_requests pieces peers;
        loop ()

    | PeerEvent (_, Peer.AvailableMetadata _) ->
        loop ()
  in
  loop ()

let make_piece m i =
  { state = Pending;
    length = Metadata.piece_length m i;
    offset = Metadata.offset m i 0;
    have = 0;
    hash = Metadata.hash m i }

let load_torrent bt m =
  Store.create (Metadata.files m) >>= fun store ->
  let pieces = Array.init (Metadata.piece_count m) (make_piece m) in
  let have = Bits.create (Metadata.piece_count m) in
  let rec loop i =
    if i < Metadata.piece_count m then begin
      Store.digest store pieces.(i).offset pieces.(i).length >>= fun sha ->
      if SHA1.equal sha pieces.(i).hash then begin
        pieces.(i).state <- Verified;
        Bits.set have i
      end;
      loop (i + 1)
    end else
      Lwt.return (store, pieces, have)
  in
  loop 0

module IncompleteMetadata = struct
  type t =
    { length : int;
      pieces : Bits.t;
      raw : Cstruct.t }

  let metadata_block_size = 1 lsl 14
  let metadata_max_size = 1 lsl 22

  let create ~length =
    let size = (length + metadata_block_size - 1) / metadata_block_size in
    { length; pieces = Bits.create size; raw = Cstruct.create length }

  let add m n buf =
    if n < 0 || n >= Bits.length m.pieces then invalid_arg "add";
    Bits.set m.pieces n;
    Cstruct.blit buf 0 m.raw (n * metadata_block_size) (Cstruct.len buf);
    Bits.has_all m.pieces

  let verify m info_hash =
    if not (Bits.has_all m.pieces) then invalid_arg "IncompleteMetadata.verify";
    if SHA1.(equal (digest m.raw) info_hash) then
      Some m.raw
    else
      None

  let iter_missing f m =
    for i = 0 to Bits.length m.pieces - 1 do
      if not (Bits.is_set m.pieces i) then
        f i
    done
end

let rec fetch_metadata bt =
  let peers = Hashtbl.create 20 in
  let peer id f = try let p = Hashtbl.find peers id in f p with Not_found -> () in
  let rec loop m =
    Lwt_stream.next bt.chan >>= fun e ->
    log_event e;
    match m, e with
    | _, PeersReceived addrs ->
        List.iter (PeerMgr.add bt.peer_mgr) addrs;
        loop m

    | _, HandshakeOk (addr, mode, fd, exts, id) ->
        PeerMgr.handshake_ok bt.peer_mgr addr id;
        let p = welcome bt.push mode fd exts id in
        Hashtbl.replace peers id p;
        loop m

    | _, HandshakeFailed addr ->
        PeerMgr.handshake_failed bt.peer_mgr addr;
        loop m

    | _, PeerConnected (addr, fd, false) ->
        connect_to_peer ~id:bt.id ~info_hash:bt.ih addr fd bt.push;
        loop m

    | _, PeerConnected (_, _, true) ->
        (* FIXME TODO FIXME *)
        loop m

    | _, PeerEvent (id, Peer.PeerDisconnected _) ->
        PeerMgr.peer_disconnected bt.peer_mgr id;
        Hashtbl.remove peers id;
        loop m

    | None, PeerEvent (id, Peer.AvailableMetadata len) ->
        if len <= IncompleteMetadata.metadata_max_size then
          let m' = IncompleteMetadata.create len in
          peer id (fun p -> IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m');
          loop (Some m')
        else begin
          Log.warn "! METADATA length %d is too large, ignoring." len;
          loop None
        end

    | Some m', PeerEvent (id, Peer.AvailableMetadata len) ->
        peer id (fun p -> IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m');
        loop m

    | _, PeerEvent (id, Peer.MetaRequested i) ->
        peer id (fun p -> Peer.reject_metadata_request p i);
        loop m

    | Some m', PeerEvent (_, Peer.GotMetaPiece (i, s)) ->
        if IncompleteMetadata.add m' i s then
          match IncompleteMetadata.verify m' bt.ih with
          | Some raw ->
              (* debug "got full metadata"; *)
              let m' = Metadata.create (Bcode.decode raw) in
              Lwt.return (m', peers)
          | None ->
              Log.error "METADATA HASH CHECK FAILED";
              loop None
        else
          loop m

    | None, PeerEvent (_, Peer.GotMetaPiece _) ->
        loop m

    | _, PeerEvent (_, Peer.GotPEX (added, dropped)) ->
        (* debug "got pex from %s added %d dropped %d" (Peer.to_string p) *)
        (*   (List.length added) (List.length dropped); *)
        List.iter (fun (addr, _) -> PeerMgr.add bt.peer_mgr addr) added;
        loop m

    | _, PeerEvent (_, Peer.DHTPort i) ->
        (* debug "got dht port %d from %s" i (Peer.to_string p); *)
        (* let addr, _ = Peer.addr p in *)
        (* Lwt.async begin fun () -> *)
        (*   DHT.ping bt.dht (addr, i) >|= function *)
        (*   | Some (id, addr) -> *)
        (*       DHT.update bt.dht Kademlia.Good id addr *)
        (*   | None -> *)
        (*       debug "%s did not reply to dht ping on port %d" (Peer.to_string p) i *)
        (* end *)
        (* FIXME *)
        loop m

    | _, PieceVerified _
    | _, PieceFailed _
    | _, TorrentComplete ->
        assert false

    | _, PeerEvent (_, Peer.Unchoked)
    | _, PeerEvent (_, Peer.Choked _)
    | _, PeerEvent (_, Peer.Interested)
    | _, PeerEvent (_, Peer.NotInterested)
    | _, PeerEvent (_, Peer.Have _)
    | _, PeerEvent (_, Peer.HaveBitfield _)
    | _, PeerEvent (_, Peer.RejectMetaPiece _)
    | _, PeerEvent (_, Peer.BlockRequested _)
    | _, PeerEvent (_, Peer.BlockReceived _) ->
        loop m
  in
  loop None

module LPD  = struct

  module Log = L.Make (struct let section = "[LPD]" end)

  let mcast_addr = "239.192.152.143"
  let mcast_port = 6771

  open Cohttp

  module Http = Request.Make (String_io.M)

  let template =
    Printf.sprintf
      "BT-SEARCH * HTTP/1.1\r\n\
       Host: %s:%u\r\n\
       Port: %u\r\n\
       Infohash: %s\r\n\
       \r\n\r\n" mcast_addr mcast_port

  let get h name =
    match Header.get h name with
    | None -> Printf.ksprintf invalid_arg "Header not found : %S" name
    | Some s -> s

  let str = Bytes.create 200

  let start fd info_hash push =
    let rec loop () =
      Lwt_unix.recvfrom fd str 0 (Bytes.length str) [] >>= fun (n, sa) ->
      let ip, port = match sa with Unix.ADDR_INET (ip, port) -> ip, port | _ -> assert false in
      let buf = { String_io.str; pos = 0; len = n } in
      match Http.read buf with
      | `Ok r when r.Request.meth = `Other "BT-SEARCH" ->
          let ih = get r.Request.headers "Infohash" in
          let p = get r.Request.headers "Port" in
          Log.info "Received announce from = %s:%d ih = %s listen = %s"
            (Unix.string_of_inet_addr ip) port ih p;
          let ih = SHA1.of_raw @@ Cstruct.of_string @@ ih in
          push (ih, (ip, int_of_string p));
          loop ()
      | `Ok r ->
          Log.error "Unxpected HTTP method : %S" (Code.string_of_method r.Request.meth);
          loop ()
      | `Invalid s ->
          Log.error "Invalid HTTP request : %S" s;
          loop ()
      | `Eof ->
          Log.error "Unexpected end of file in HTTP request ";
          loop ()
    in
    loop ()

  let start fd info_hash push =
    Lwt.catch
      (fun () -> start fd info_hash push)
      (fun e ->
         Lwt.wrap2 Log.error "loop exn : %S ; restarting ..." (Printexc.to_string e))

  let start fd info_hash push =
    let rec loop () = start fd info_hash push >>= loop in
    loop ()

  let announce_delay = 5. *. 60.

  let announce fd port info_hash =
    let msg = template port @@ SHA1.to_hex info_hash in
    let sa = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string mcast_addr, mcast_port) in
    let rec loop () =
      Lwt_unix.sendto fd msg 0 (String.length msg) [] sa >>= fun _ ->
      Lwt_unix.sleep announce_delay >>=
      loop
    in
    loop ()

  let announce fd port info_hash =
    Lwt.catch
      (fun () -> announce fd port info_hash)
      (fun e ->
         Lwt.wrap2 Log.error "announce exn : %s ; restarting ..." (Printexc.to_string e))

  let announce fd port info_hash =
    let rec loop () = announce fd port info_hash >>= loop in
    loop ()

  let start ~port ~info_hash push =
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    let fd2 = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
    Lwt_unix.setsockopt fd2 Lwt_unix.SO_REUSEADDR true;
    Lwt_unix.mcast_set_loop fd2 false;
    Lwt_unix.bind fd (Lwt_unix.ADDR_INET (Unix.inet_addr_any, mcast_port));
    Log.debug "Joining multicast group %s:%d" mcast_addr mcast_port;
    Lwt_unix.mcast_add_membership fd (Unix.inet_addr_of_string mcast_addr);
    Lwt.ignore_result (start fd info_hash push);
    Lwt.ignore_result (announce fd2 port info_hash)

end

let start_server ?(port = 0) push =
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
  let port = match Lwt_unix.getsockname fd with Unix.ADDR_INET (_, p) -> p | Unix.ADDR_UNIX _ -> assert false in
  Lwt_unix.listen fd listen_backlog;
  Log.debug "listening on port %u" port;
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, sa) ->
    Log.debug "accepted connection from %s" (Util.string_of_sockaddr sa);
    let addr = match sa with Unix.ADDR_INET (ip, p) -> ip, p | Unix.ADDR_UNIX _ -> assert false in
    push (PeerConnected (addr, fd, true));
    loop ()
  in
  loop ()

let start_server ?port push =
  Lwt.ignore_result (start_server ?port push)

let start bt =
  start_server bt.push;
  List.iter (Tracker.announce ~info_hash:bt.ih (fun peers -> bt.push (PeersReceived peers)) bt.id) bt.trackers;
  fetch_metadata bt >>= fun (m, peers) ->
  load_torrent bt m >>= fun (store, pieces, have) ->
  Log.info "Torrent loaded have %d/%d pieces" (Bits.count_ones have) (Bits.length have);
  share_torrent bt m store pieces have peers

let create mg =
  let ch, push = let ch, push = Lwt_stream.create () in ch, (fun x -> push (Some x)) in
  let peer_mgr = PeerMgr.create (fun addr fd -> push (PeerConnected (addr, fd, false))) in
  { id = SHA1.generate ~prefix:"OCAML" ();
    ih = mg.Magnet.xt;
    trackers = mg.Magnet.tr;
    chan = ch;
    push;
    peer_mgr }
