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
module Log  = Log.Make (struct let section = "Client" end)
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
  | PeerConnected of (ARC4.key * ARC4.key) option * Lwt_unix.file_descr * Bits.t * SHA1.t
  | IncomingConnection of Lwt_unix.file_descr * Unix.sockaddr
  | PieceVerified of int
  | PieceFailed of int
  | ConnectFailed of addr
  | TorrentComplete
  | PeerEvent of SHA1.t * Peer.event

type t =
  { id : SHA1.t;
    ih : SHA1.t;
    trackers : Uri.t list;
    peer_mgr : PeerMgr.swarm;
    chan : event Lwt_stream.t;
    push : event -> unit }

let proto = Cstruct.of_string "\019BitTorrent protocol"

let extensions =
  let bits = Bits.create (8 * 8) in
  (* Bits.set bits Wire.ltep_bit; *)
  (* Bits.set bits Wire.dht_bit; *)
  Bits.to_cstruct bits

let handshake_len =
  Cstruct.len proto + 8 (* extensions *) + 20 (* info_hash *) + 20 (* peer_id *)

let handshake_message peer_id info_hash =
  Cs.concat [ proto; extensions; SHA1.to_raw info_hash; SHA1.to_raw peer_id ]

let parse_handshake cs =
  assert (Cstruct.len cs = handshake_len);
  match Cstruct.get_uint8 cs 0 with
  | 19 ->
      let proto' = Cstruct.sub cs 0 20 in
      if Cs.equal proto' proto then
        let ext = Cstruct.sub cs 20 8 in
        let info_hash = Cstruct.sub cs 28 20 in
        let peer_id = Cstruct.sub cs 48 20 in
        (ext, SHA1.of_raw info_hash, SHA1.of_raw peer_id)
      else
        failwith "bad proto"
  | _ ->
      failwith "bad proto length"

let buf_size = 1024

let negotiate fd t =
  let read_buf = Cstruct.create buf_size in
  let rec loop = function
    | `Ok (t, Some cs) ->
        Lwt_cstruct.complete (Lwt_cstruct.write fd) cs >>= fun () ->
        loop (Handshake.handle t Cs.empty)
    | `Ok (t, None) ->
        Lwt_cstruct.read fd read_buf >>= begin function
        | 0 ->
            failwith "eof"
        | n ->
            loop (Handshake.handle t (Cstruct.sub read_buf 0 n))
        end
    | `Error err ->
        failwith err
    | `Success (mode, rest) ->
        Lwt.return (`Ok (mode, rest))
  in
  loop t

let connect_to_peer info_hash ip port timeout =
  let my_id = SHA1.generate () in (* FIXME FIXME *)
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.gethostbyaddr ip >>= fun he ->
  let sa = Lwt_unix.ADDR_INET (he.Lwt_unix.h_addr_list.(0), port) in
  Lwt_unix.connect fd sa >>= fun () ->
  negotiate fd Handshake.(outgoing ~info_hash Both) >>= function
  | `Ok (mode, rest) ->
      Lwt_cstruct.complete (Lwt_cstruct.write fd) (handshake_message info_hash my_id) >>= fun () ->
      assert (Cstruct.len rest <= handshake_len);
      let hs = Cstruct.create handshake_len in
      Cstruct.blit rest 0 hs 0 (Cstruct.len rest);
      Lwt_cstruct.complete (Lwt_cstruct.read fd) (Cstruct.shift hs (Cstruct.len rest)) >>= fun () ->
      let ext, info_hash', peer_id = parse_handshake hs in
      assert (SHA1.equal info_hash info_hash');
      Lwt.return (mode, fd, ext, peer_id)
  | `Error _ ->
      assert false

let connect_to_peer info_hash push = function
  | Some (addr, timeout) ->
      let ip, port = addr in
      ignore
        (Lwt.try_bind
           (fun () -> connect_to_peer info_hash ip port timeout)
           (fun (mode, fd, ext, peer_id) ->
              Lwt.wrap1 push @@ PeerConnected (mode, fd, Bits.of_cstruct ext, peer_id))
           (fun _ -> Lwt.wrap1 push @@ ConnectFailed addr))
  | None -> ()

let am_choking peers id =
  try
    let p = Hashtbl.find peers id in
    Peer.am_choking p
  with
  | Not_found -> true

let peer_interested peers id =
  try
    let p = Hashtbl.find peers id in
    Peer.peer_interested p
  with
  | Not_found -> false

let welcome push mode fd exts id =
  let p = Peer.create id (fun e -> push (PeerEvent (id, e))) fd mode in
  Peer.extended_handshake p;
  (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)
  p

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
            (p, Random.int max_int) :: wires
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

(* Requester END *)

type piece_state =
  | Pending
  | Active of int array
  | Finished

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
      | Finished
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
      | Finished ->
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
      | Finished
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
  Hashtbl.iter (fun _ p -> request p pieces) peers

let send_block store pieces p i off len =
  match pieces.(i).state with
  | Finished ->
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
  | Finished ->
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
  | Finished
  | Pending -> ()
  | Active parts ->
      let j = off / block_size in
      if parts.(j) > 0 then parts.(j) <- parts.(j) - 1

let record_block store peers pieces i off s push =
  Lwt.ignore_result (record_block store peers pieces i off s push)

let share_torrent bt meta store pieces have peers =
  let peer id f = try let p = Hashtbl.find peers id in f p with Not_found -> () in
  Hashtbl.iter (fun _ p -> Peer.have_bitfield p have) peers;
  update_requests pieces peers;
  let rec loop () =
    Lwt_stream.next bt.chan >>= function
    | TorrentComplete ->
        (* FIXME TODO FIXME *)
        Lwt.return_unit

    | PeersReceived addrs ->
        (* debug "received %d peers" (List.length addrs); *)
        List.iter (fun addr -> connect_to_peer bt.ih bt.push (PeerMgr.add bt.peer_mgr addr)) addrs;
        loop ()

    | PieceVerified i ->
        (* debug "piece %d verified and written to disk" i; *)
        Hashtbl.iter (fun _ p -> Peer.have p i) peers;
        loop ()

    | PieceFailed i ->
        (* debug "piece %d failed hashcheck" i; *)
        (* FIXME FIXME *)
        loop ()

    | ConnectFailed addr ->
        connect_to_peer bt.ih bt.push (PeerMgr.handshake_failed bt.peer_mgr addr);
        loop ()

    | IncomingConnection (fd, sa) ->
        (* FIXME TODO FIXME *)
        loop ()

    | PeerEvent (id, Peer.PeerDisconnected reqs) ->
        List.iter (request_rejected pieces) reqs;
        connect_to_peer bt.ih bt.push (PeerMgr.peer_disconnected bt.peer_mgr id);
        Hashtbl.remove peers id;
        (* if not (am_choking peers id) && peer_interested peers id then Choker.rechoke ch; *)
        loop ()

    | PeerEvent (_, Peer.Choked reqs) ->
        List.iter (request_rejected pieces) reqs;
        loop ()

    | PeerEvent (_, Peer.Unchoked)
    | PeerEvent (_, Peer.Have _)
    | PeerEvent (_, Peer.HaveBitfield _) ->
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
        peer id (fun p -> send_block store pieces p i off len);
        loop ()

    | PeerEvent (_, Peer.BlockReceived (i, off, s)) ->
        record_block store peers pieces i off s bt.push;
        (* debug "got block %d/%d (piece %d) from %s" b *)
        (*   (Metadata.block_count meta idx) idx (Peer.to_string p); *)
        (* (Util.string_of_file_size (Int64.of_float (Peer.download_rate p))); *)
        loop ()

    | PeerEvent (_, Peer.GotPEX (added, dropped)) ->
        (* debug "got pex from %s added %d dropped %d" (Peer.to_string p) *)
        (*   (List.length added) (List.length dropped); *)
        List.iter (fun (addr, _) -> connect_to_peer bt.ih bt.push (PeerMgr.add bt.peer_mgr addr)) added;
        loop ()

    | PeerEvent (_, Peer.DHTPort _) ->
        (* debug "got dht port %d from %s" i (Peer.to_string p); *)
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

    | PeerConnected (mode, sock, exts, id) ->
        let p = welcome bt.push mode sock exts id in
        (* let p = Peer.create_has_meta id meta (\*  (Requester.get_next_requests r) *\) in *)
        (* Lwt.async (fun () -> reader_loop bt.push sock p); *)
        (* if Bits.is_set exts Wire.ltep_bit then Peer.send_extended_handshake p; *)
        (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)
        (* (\* Peer.send_have_bitfield p (Torrent.have tor) *\) *)
        (* (\* FIXME *\) *)
        Hashtbl.replace peers id p;
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
      if SHA1.equal sha pieces.(i).hash then
        (pieces.(i).state <- Finished; Bits.set have i);
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
    match m, e with
    | _, PeersReceived addrs ->
        (* debug "received %d peers" (List.length addrs); *)
        List.iter (fun addr -> connect_to_peer bt.ih bt.push (PeerMgr.add bt.peer_mgr addr)) addrs;
        loop m

    | _, PeerConnected (mode, fd, exts, id) ->
        let p = welcome bt.push mode fd exts id in
        Hashtbl.replace peers id p;
        loop m

    | _, ConnectFailed addr ->
        connect_to_peer bt.ih bt.push (PeerMgr.handshake_failed bt.peer_mgr addr);
        loop m

    | _, IncomingConnection (fd, sa) ->
        (* FIXME TODO FIXME *)
        loop m

    | _, PeerEvent (id, Peer.PeerDisconnected _) ->
        connect_to_peer bt.ih bt.push (PeerMgr.peer_disconnected bt.peer_mgr id);
        Hashtbl.remove peers id;
        loop m

    | None, PeerEvent (id, Peer.AvailableMetadata len) ->
        (* debug "%s offered %d bytes of metadata" (Peer.to_string p) len; *)
        (* FIXME *)
        let m' = IncompleteMetadata.create len in
        peer id (fun p -> IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m');
        loop (Some m')

    | Some m', PeerEvent (id, Peer.AvailableMetadata len) ->
        peer id (fun p -> IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m');
        loop m

    | _, PeerEvent (id, Peer.MetaRequested i) ->
        peer id (fun p -> Peer.reject_metadata_request p i);
        loop m

    | Some m', PeerEvent (_, Peer.GotMetaPiece (i, s)) ->
        (* debug "got metadata piece %d/%d from %s" i *)
        (*   (IncompleteMetadata.piece_count m) (Peer.to_string p); *)
        if IncompleteMetadata.add m' i s then
          match IncompleteMetadata.verify m' bt.ih with
          | Some raw ->
              (* debug "got full metadata"; *)
              let m' = Metadata.create (Bcode.decode raw) in
              Lwt.return (m', peers)
          | None ->
              (* debug "metadata hash check failed; trying again"; *)
              loop None
        else
            loop m

    | None, PeerEvent (_, Peer.GotMetaPiece _) ->
        loop m

    | _, PeerEvent (_, Peer.GotPEX (added, dropped)) ->
        (* debug "got pex from %s added %d dropped %d" (Peer.to_string p) *)
        (*   (List.length added) (List.length dropped); *)
        List.iter (fun (addr, _) -> connect_to_peer bt.ih bt.push (PeerMgr.add bt.peer_mgr addr)) added;
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

  module Log = L.Make (struct let section = "LPD" end)

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
    Log.debug "LPD.start";
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    let fd2 = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
    Lwt_unix.setsockopt fd2 Lwt_unix.SO_REUSEADDR true;
    Lwt_unix.mcast_set_loop fd2 false;
    Lwt_unix.bind fd (Lwt_unix.ADDR_INET (Unix.inet_addr_any, mcast_port));
    Log.debug "Joining multicast group...";
    Lwt_unix.mcast_add_membership fd (Unix.inet_addr_of_string mcast_addr);
    Lwt.ignore_result (start fd info_hash push);
    Lwt.ignore_result (announce fd2 port info_hash)

end

let start bt =
  List.iter (Tracker.announce ~info_hash:bt.ih (fun peers -> bt.push (PeersReceived peers)) bt.id) bt.trackers;
  fetch_metadata bt >>= fun (m, peers) ->
  load_torrent bt m >>= fun (store, pieces, have) ->
  share_torrent bt m store pieces have peers

let start_server ?(port = 0) push =
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, 0));
  Lwt_unix.listen fd listen_backlog;
  (* debug "listening on port %u" port; *)
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, sa) ->
    (* debug "accepted connection from %s" (Util.string_of_sockaddr sa); *)
    push (IncomingConnection (fd, sa));
    loop ()
  in
  loop ()

let start_server ?port push =
  Lwt.ignore_result (start_server ?port push)

let create mg =
  let ch, push = let ch, push = Lwt_stream.create () in ch, (fun x -> push (Some x)) in
  let peer_mgr = PeerMgr.create () in
  start_server push;
  { id = SHA1.generate ~prefix:"OCAML" ();
    ih = mg.Magnet.xt;
    trackers = mg.Magnet.tr;
    chan = ch;
    push;
    peer_mgr }
