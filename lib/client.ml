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
  | ConnectToPeer of addr * float
  | IncomingPeer of addr * Util.Socket.t
  | PieceVerified of int
  | PieceFailed of int
  | HandshakeFailed of addr
  | HandshakeOk of addr * Util.Socket.t * Bits.t * SHA1.t
  | TorrentComplete
  | PeerEvent of SHA1.t * Peer.event
  | Torrent_loaded of Store.t
  | Store_error of string

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

type waiting =
  IncompleteMetadata.t option

type loading =
  { pieces : Pieces.piece array;
    metadata : Metadata.t;
    have : Bits.t }

type sharing =
  { pieces : Pieces.piece array;
    metadata : Metadata.t;
    have : Bits.t;
    store : Store.t  }

type seeding =
  { metadata : Metadata.t;
    store : Store.t }

type state =
  | Waiting of waiting
  | Loading of loading
  | Sharing of sharing
  | Seeding of seeding

type t =
  { id : SHA1.t;
    ih : SHA1.t;
    trackers : Uri.t list;
    peer_mgr : PeerMgr.swarm;
    chan : event Lwt_stream.t;
    peers : (SHA1.t, Peer.t) Hashtbl.t;
    push : event -> unit }

(* let pex_delay = 60.0 *)

(* let rec pex_pulse pm = *)
(*   let pex = Hashtbl.fold (fun addr _ l -> addr :: l) pm.peers [] in *)
(*   iter_peers (fun p -> Peer.send_pex p pex) pm; *)
(*   Lwt_unix.sleep pex_delay >>= fun () -> pex_pulse pm *)

let connect_to_peer ~id ~info_hash addr timeout push =
  let push = function
    | Handshake.Ok (sock, ext, peer_id) ->
       Log.info "Connected to %s:%d [%a] successfully" (Unix.string_of_inet_addr (fst addr)) (snd addr)
         SHA1.print_hex_short peer_id;
       push @@ HandshakeOk (addr, sock, ext, peer_id)
    | Handshake.Failed ->
       Log.error "Connection to %s:%d failed" (Unix.string_of_inet_addr (fst addr)) (snd addr);
       push @@ HandshakeFailed addr
  in
  (Lwt_unix.sleep timeout >>= fun () -> Handshake.outgoing ~id ~info_hash addr push; Lwt.return_unit) |>
  Lwt.ignore_result

let welcome push sock exts id =
  Peer.create id (fun e -> push (PeerEvent (id, e))) sock
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
    (* Log.info "RECHOKING"; *)
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
    (* Log.debug "RECHOKE %d TOTAL" (List.length wires); *)
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
    (* Log.debug "RECHOKE total=%d unchoke=%d choke=%d" (Hashtbl.length peers) (List.length unchoke) *)
    (*   (List.length choke); *)
    List.iter (fun (p, _) -> Peer.unchoke p) unchoke;
    List.iter (fun (p, _) -> Peer.choke p) choke;
    Lwt_unix.sleep choke_timeout >>= fun () -> loop opt nopt
  in
  loop None rechoke_optimistic_duration

let load_torrent metadata k =
  let pieces = Array.init (Metadata.piece_count metadata) (Pieces.make_piece metadata) in
  let have = Bits.create (Metadata.piece_count metadata) in
  Store.create (Metadata.files metadata) (function
    | `Ok store ->
        let rec loop i =
          if i < Metadata.piece_count metadata then
            Store.digest store pieces.(i).Pieces.offset pieces.(i).Pieces.length (function
              | `Ok sha ->
                  if SHA1.equal sha pieces.(i).Pieces.hash then begin
                    pieces.(i).Pieces.state <- Pieces.Verified;
                    Bits.set have i
                  end;
                  loop (i + 1)
              | `Error s ->
                  k (`Error s)
            )
          else
            k (`Ok store)
        in
        loop 0
    | `Error s ->
        k (`Error s)
  );
  (pieces, have)

let process_peer_event client p e state =
  match state, e with
  | Sharing {pieces; _}, Peer.PeerDisconnected reqs ->
      List.iter (Pieces.request_rejected pieces) reqs;
      PeerMgr.peer_disconnected client.peer_mgr (Peer.id p);
      Hashtbl.remove client.peers (Peer.id p);
      state

  | (Sharing _ | Seeding _ | Loading _), Peer.AvailableMetadata _ ->
      state

  | (Waiting _ | Loading _ | Seeding _), Peer.PeerDisconnected reqs ->
      PeerMgr.peer_disconnected client.peer_mgr (Peer.id p);
      Hashtbl.remove client.peers (Peer.id p);
      (* if not (am_choking peers id) && peer_interested peers id then Choker.rechoke ch; *)
      state

  | Sharing {pieces; _}, Peer.Choked reqs ->
      List.iter (Pieces.request_rejected pieces) reqs;
      state

  | Waiting _, (Peer.Choked _ | Peer.Unchoked | Peer.Have _)
  | Waiting _, (Peer.HaveBitfield _ | Peer.BlockRequested _ | Peer.BlockReceived _) ->
      state

  | Sharing {pieces; _}, (Peer.Unchoked | Peer.Have _ | Peer.HaveBitfield _) ->
      Pieces.update_interest pieces p;
      Pieces.update_requests pieces client.peers;
      state

  | _, (Peer.Interested | Peer.NotInterested) ->
      (* rechoke *)
      state

  | (Sharing {metadata; _} | Seeding {metadata; _} | Loading {metadata; _}), Peer.MetaRequested i ->
      Peer.metadata_piece (Metadata.length metadata) i (Metadata.get_piece metadata i) p;
      state

  | Waiting _, Peer.MetaRequested i ->
      Peer.reject_metadata_request p i;
      state

  | _, Peer.RejectMetaPiece _ -> (* CHECK *)
      state

  | (Waiting None | Sharing _ | Seeding _), Peer.GotMetaPiece _ ->
      state

  | Waiting (Some m), Peer.GotMetaPiece (i, s) ->
      if IncompleteMetadata.add m i s then
        match IncompleteMetadata.verify m client.ih with
        | Some raw ->
            (* debug "got full metadata"; *)
            let metadata = Metadata.create (Bcode.decode raw) in
            let pieces, have = load_torrent metadata (function
                | `Ok store ->
                    client.push (Torrent_loaded store)
                | `Error s ->
                    client.push (Store_error s)
              )
            in
            Loading {metadata; pieces; have}
        | None ->
            Log.error "METADATA HASH CHECK FAILED";
            Waiting None
      else
        state

  | Sharing {pieces; store; _}, Peer.BlockRequested (i, off, len) ->
      if i >= 0 && i < Array.length pieces then begin
        Pieces.send_block store pieces p i off len;
        state
      end else begin
        Log.warn "! PEER REQUEST invalid piece:%d" i;
        state
      end

  | Sharing {store; pieces; _}, Peer.BlockReceived (i, off, s) ->
      if i >= 0 && i < Array.length pieces then begin
        Pieces.record_block store client.peers pieces i off s (function
          | `Ok -> () (* client.push TODO *)
          | `Piece_complete -> () (* TODO TODO *)
          | `Error s -> client.push (Store_error s)
        );
        state
      end else begin
        Log.warn "! PIECE invalid piece:%d" i;
        state
      end

  | _, Peer.GotPEX (added, dropped) ->
      List.iter (fun (addr, _) -> PeerMgr.add client.peer_mgr addr) added;
      state

  | _, Peer.DHTPort _ ->
      (* let addr, _ = Peer.addr p in *)
      (* Lwt.async begin fun () -> *)
      (*   DHT.ping bt.dht (addr, i) >|= function *)
      (*   | Some (id, addr) -> *)
      (*       DHT.update bt.dht Kademlia.Good id addr *)
      (*   | None -> *)
      (*       debug "%s did not reply to dht ping on port %d" (Peer.to_string p) i *)
      (* end; *)
      (* FIXME *)
      state

  | Waiting (Some m), Peer.AvailableMetadata len ->
      IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m;
      state

  | Waiting None, Peer.AvailableMetadata len ->
      if len <= IncompleteMetadata.metadata_max_size then begin
        let m = IncompleteMetadata.create len in
        IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m;
        Waiting (Some m)
      end else begin
        Log.warn "! METADATA length %d is too large, ignoring." len;
        state
      end

let process_event client e state =
  (* Log.info "TORRENT SHARE have:%d total:%d peers:%d" *)
  (*   (Bits.count_ones have) (Bits.length have) (Hashtbl.length peers); *)
  match state, e with
  | _, PeersReceived addrs ->
      List.iter (PeerMgr.add client.peer_mgr) addrs;
      state

  | Loading {pieces; have; metadata}, Torrent_loaded store ->
      Hashtbl.iter (fun _ p ->
        Peer.have_bitfield p have;
        Pieces.update_interest pieces p
      ) client.peers;
      Pieces.update_requests pieces client.peers;
      Lwt.ignore_result (rechoke client.peers);
      Sharing {pieces; have; metadata; store} (* CHECK *)

  | Sharing {pieces; have; _}, PieceVerified i ->
      pieces.(i).Pieces.state <- Pieces.Verified;
      Bits.set have i;
      Hashtbl.iter (fun _ p -> Peer.have p i) client.peers;
      state

  | Waiting _, (TorrentComplete | Torrent_loaded _ | Store_error _) ->
      assert false

  | (Waiting _ | Loading _ | Seeding _), PieceVerified _ ->
      assert false
  (* maybe update interest ? *)
  (* check if done ! *)

  | _, PieceFailed i ->
      (* FIXME FIXME *)
      state

  | _, HandshakeFailed addr ->
      PeerMgr.handshake_failed client.peer_mgr addr;
      state

  | _, PeerEvent (id, e) ->
      if Hashtbl.mem client.peers id then
        let p = Hashtbl.find client.peers id in
        process_peer_event client p e state
      else begin
        Log.warn "! Errant message from peer %a" SHA1.print_hex_short id;
        state
      end

  | _, ConnectToPeer (addr, timeout) ->
      connect_to_peer ~id:client.id ~info_hash:client.ih addr timeout client.push;
      state

  | _, IncomingPeer _ ->
      (* FIXME FIXME *)
      state

  | Sharing {pieces; have; _}, HandshakeOk (addr, sock, exts, id) ->
      PeerMgr.handshake_ok client.peer_mgr addr id;
      let p = welcome client.push sock exts id in
      Peer.have_bitfield p have;
      Hashtbl.replace client.peers id p;
      Pieces.update_interest pieces p;
      Pieces.update_requests pieces client.peers;
      state

  | (Waiting _ | Loading _ | Seeding _), HandshakeOk (addr, sock, exts, id) ->
      PeerMgr.handshake_ok client.peer_mgr addr id;
      let p = welcome client.push sock exts id in
      Hashtbl.replace client.peers id p;
      state

let upkeep client =
  PeerMgr.upkeep client.peer_mgr

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
    push (IncomingPeer (addr, Util.Socket.tcp fd));
    loop ()
  in
  loop ()

let start_server ?port push =
  Lwt.ignore_result (start_server ?port push)

let start client =
  start_server client.push;
  List.iter (Tracker.announce ~info_hash:client.ih (fun peers ->
      client.push (PeersReceived peers)) client.id
  ) client.trackers;
  Lwt_stream.fold (process_event client) client.chan (Waiting None) >>= fun _ ->
  Lwt.return_unit

let create mg =
  let ch, push = let ch, push = Lwt_stream.create () in ch, (fun x -> push (Some x)) in
  let peer_mgr = PeerMgr.create (fun addr timeout -> push @@ ConnectToPeer (addr, timeout)) in
  { id = SHA1.generate ~prefix:"OCAML" ();
    ih = mg.Magnet.xt;
    trackers = mg.Magnet.tr;
    chan = ch;
    push;
    peer_mgr;
    peers = Hashtbl.create 20 }
