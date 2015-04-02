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

let (>>=) = Lwt.(>>=)

let listen_ports = [50000]
let choke_timeout = 5.
let rechoke_interval = 10.
let rechoke_optimistic_duration = 2
let rechoke_slots = 10

open Event

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
  Bits.set bits Wire.ltep_bit;
  Bits.set bits Wire.dht_bit;
  Cstruct.of_string @@ Bits.to_bin bits

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

let encrypt mode cs =
  match mode with
  | `Plain -> `Plain, cs
  | `Encrypted (my_key, her_key) ->
      let { ARC4.key = my_key; message = cs } = ARC4.encrypt ~key:my_key cs in
      `Encrypted (my_key, her_key), cs

let decrypt mode cs =
  match mode with
  | `Plain -> `Plain, cs
  | `Encrypted (my_key, her_key) ->
      let { ARC4.key = my_key; message = cs } = ARC4.decrypt ~key:her_key cs in
      `Encrypted (my_key, her_key), cs

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

let connect_to_peer info_hash push ((ip, port) as addr) timeout =
  ignore
    (Lwt.try_bind
       (fun () -> connect_to_peer info_hash ip port timeout)
       (fun (mode, fd, ext, peer_id) ->
          Lwt.wrap1 push @@ PeerConnected (mode, fd, Bits.of_bin (Cstruct.to_string ext), peer_id))
       (fun _ -> Lwt.wrap1 push @@ ConnectFailed addr))

let buf_size = 1024

let reader_loop push fd p =
  let buf = Cstruct.create buf_size in
  let rec loop r =
    Lwt_unix.with_timeout Peer.keepalive_delay
      (fun () -> Lwt_cstruct.read fd buf) >>= function
    | 0 ->
        failwith "eof"
    | n ->
        let r, msgs = Wire.R.handle r (Cstruct.sub buf 0 n) in
        List.iter (fun msg -> push (Peer.got_message p msg)) msgs;
        loop r
  in
  Lwt.catch
    (fun () -> loop Wire.R.empty)
    (fun e ->
       Printf.eprintf "unexpected exc: %S\n%!" (Printexc.to_string e);
       push (PeerDisconnected (Peer.id p));
       Lwt_unix.close fd)

let writer_loop fd =
  let ss, send = Lwt_stream.create () in
  let rec loop () =
    Lwt.pick
      [ (Lwt_stream.next ss);
        (Lwt_unix.sleep Peer.keepalive_delay >>= fun () -> Lwt.return Wire.KEEP_ALIVE) ]
    >>= fun m ->
    Lwt_cstruct.complete
      (Lwt_cstruct.write fd) (Util.W.to_cstruct @@ Wire.writer m) >>= loop
  in
  loop (), (fun x -> send (Some x))

  (* | TorrentLoaded dl -> *)
  (*         (\* debug "torrent loaded (good=%d,total=%d)" *\) *)
  (*         (\*   (Torrent.numgot dl) (Metadata.piece_count meta - Torrent.numgot dl); *\) *)
  (*         (\* PeerMgr.torrent_loaded bt.peer_mgr meta dl (get_next_requests bt); FIXME FIXME *\) *)
  (*         let ch = Choker.create bt.peer_mgr dl in *)
  (*         if Torrent.is_complete dl then *)
  (*           bt.stage <- HasMeta (meta, Seeding (dl, ch)) *)
  (*         else begin *)
  (*           let r = Requester.create meta dl in *)
  (*           bt.stage <- HasMeta (meta, Leeching (dl, ch, r)); *)
  (*           PeerMgr.iter_peers (fun p -> Requester.got_bitfield r (Peer.have p)) bt.peer_mgr *)
  (*         end; *)
  (*         Choker.start ch *)
(*     end *)

let max_datagram_size = 1024

let announce ~info_hash url push id =
  let read_buf = Cstruct.create max_datagram_size in
  let rec loop fd = function
    | `Ok (t, (timeout, buf)) ->
        Lwt_cstruct.write fd buf >>= fun _ ->
        Lwt_unix.with_timeout timeout (fun () -> Lwt_cstruct.read fd read_buf) >>= fun n ->
        loop fd (Tracker.handle t (Cstruct.sub read_buf 0 n))

    | `Error s ->
        Printf.ksprintf failwith "tracker error %S" s

    | `Success (interval, leechers, seeders, peers) ->
        push (PeersReceived peers);
        Lwt_unix.sleep interval >>= fun () -> loop fd (Tracker.create ~info_hash ~id `Udp)
  in
  match Uri.scheme url with
  | Some "udp" ->
      let h = match Uri.host url with None -> assert false | Some h -> h in
      let p = match Uri.port url with None -> assert false | Some p -> p in
      Lwt_unix.gethostbyname h >>= fun he ->
      let ip = he.Unix.h_addr_list.(0) in
      let fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
      let sa = Lwt_unix.ADDR_INET (ip, p) in
      Lwt_unix.connect fd sa >>= fun () ->
      loop fd (Tracker.create ~info_hash ~id `Udp)
  | Some s ->
      Printf.ksprintf failwith "tracker scheme %S not supported" s
  | None ->
      failwith "no tracker scheme"

let announce ~info_hash url push id =
  Lwt.catch
    (fun () -> announce ~info_hash url push id)
    (fun exn -> (* debug ~exn "announce failure"; *) Lwt.return_unit)

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
  let p = Peer.create_no_meta id in
  let _ = reader_loop push fd p in
  let _, send = writer_loop fd in
  send @@ Peer.extended_handshake p;
  (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)
  p, send

let rechoke_compare (p1, _, salt1) (p2, _, salt2) =
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
      let add _ (p, send) wires =
        match Peer.is_seeder p, opt with
        | true, _ ->
            send (Peer.choke p);
            wires
        | false, Some opt when SHA1.equal (Peer.id p) opt ->
            wires
        | false, _ ->
            (p, send, Random.int max_int) :: wires
      in
      Hashtbl.fold add peers []
    in

    let wires = List.sort rechoke_compare wires in

    let rec select n acc = function

      | (p, _, _) as w :: wires when n < rechoke_slots ->
          let n = if Peer.peer_interested p then n + 1 else n in
          select n (w :: acc) wires

      | wires ->
          begin match opt with
          | Some _ ->
              acc, wires, opt, nopt

          | None ->
              let wires = List.filter (fun (p, _, _) -> Peer.peer_interested p) wires in
              if List.length wires > 0 then
                let (p, _, _) as opt = List.nth wires (Random.int (List.length wires)) in
                (opt :: acc), wires, Some (Peer.id p), rechoke_optimistic_duration
              else
                acc, wires, None, nopt
          end

    in

    let unchoke, choke, opt, nopt = select 0 [] wires in

    List.iter (fun (p, send, _) -> send (Peer.unchoke p)) unchoke;
    List.iter (fun (p, send, _) -> send (Peer.choke p)) choke;

    Lwt_unix.sleep choke_timeout >>= fun () -> loop opt nopt
  in

  loop None rechoke_optimistic_duration

let share_torrent bt meta dl peers =
  let send id m = try let p, send = Hashtbl.find peers id in send (m p) with Not_found -> () in
  (* let r = Requester.create meta dl in *)
  (* Hashtbl.iter (fun _ p -> Requester.got_bitfield r (Peer.has p)) peers; *)
  let rec loop () =
    Lwt_stream.next bt.chan >>= function
    | PeersReceived addrs ->
        (* debug "received %d peers" (List.length addrs); *)
        List.iter (fun addr -> bt.push (PeerMgr.add bt.peer_mgr addr)) addrs;
        loop ()

    | PieceVerified i ->
        (* debug "piece %d verified and written to disk" i; *)
        (* PeerMgr.got_piece bt.peer_mgr i; *)
        (* Requester.got_piece r i; *)
        loop ()

    | PieceFailed i ->
        (* Announcer.add_bytes *)
        (* debug "piece %d failed hashcheck" i; *)
        (* Requester.got_bad_piece r i; *)
        (* PeerMgr.got_bad_piece bt.peer_mgr i; *)
        loop ()

    | ConnectFailed addr ->
        bt.push (PeerMgr.handshake_failed bt.peer_mgr addr);
        loop ()

    | PeerDisconnected id ->
        bt.push (PeerMgr.peer_disconnected bt.peer_mgr id);
        (* if not (am_choking peers id) && peer_interested peers id then Choker.rechoke ch; *)
        (* Requester.peer_declined_all_requests r id; FIXME FIXME *)
        (* Requester.lost_bitfield r (Peer.have p); FIXME FIXME *)
        loop ()

    | ConnectPeer (addr, timeout) ->
        connect_to_peer bt.ih bt.push addr timeout;
        loop ()

    | Choked _ ->
        (* Requester.peer_declined_all_requests r p; FIXME FIXME *)
        loop ()

    | Unchoked _ ->
        loop ()

    | Interested id
    | NotInterested id ->
        (* if not (am_choking peers id) then Choker.rechoke ch; *)
        loop ()

    | Have (p, i) ->
        (* Requester.got_have r i; *)
        loop ()

    | HaveBitfield (p, b) ->
        (* Requester.got_bitfield r b; *)
        loop ()

    | MetaRequested (id, i) ->
        send id
          (Peer.metadata_piece (Metadata.length meta) i (Metadata.get_piece meta i));
        loop ()

    | GotMetaPiece _
    | RejectMetaPiece _ ->
        loop ()

    | BlockRequested (p, idx, b) ->
        (* if Torrent.has_piece dl idx then begin *)
        (*   let aux _ = Torrent.get_block dl idx b >|= Peer.send_block p idx b in *)
        (*   Lwt.async aux *)
        (* end; *)
        (* FIXME FIXME *)
        loop ()

    | BlockReceived (p, idx, b, s) ->
        (* FIXME *)
        (* debug "got block %d/%d (piece %d) from %s" b *)
        (*   (Metadata.block_count meta idx) idx (Peer.to_string p); *)
        (* (Util.string_of_file_size (Int64.of_float (Peer.download_rate p))); *)
        (* Requester.got_block r p idx b; *)
        (* Torrent.got_block t p idx b s; *)
        (* FIXME FIXME *)
        loop ()

    | GotPEX (p, added, dropped) ->
        (* debug "got pex from %s added %d dropped %d" (Peer.to_string p) *)
        (*   (List.length added) (List.length dropped); *)
        List.iter (fun (addr, _) -> bt.push (PeerMgr.add bt.peer_mgr addr)) added;
        loop ()

    | DHTPort _ ->
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
        let p, send = welcome bt.push mode sock exts id in
        (* let p = Peer.create_has_meta id meta (\*  (Requester.get_next_requests r) *\) in *)
        (* Lwt.async (fun () -> reader_loop bt.push sock p); *)
        (* if Bits.is_set exts Wire.ltep_bit then Peer.send_extended_handshake p; *)
        (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)
        (* (\* Peer.send_have_bitfield p (Torrent.have tor) *\) *)
        (* (\* FIXME *\) *)
        Hashtbl.replace peers id (p, send);
        loop ()

    | AvailableMetadata _
    | NoEvent ->
        loop ()
  in
  loop ()

let load_torrent bt meta =
  Torrent.create meta bt.push (* >|= fun dl -> *)
(* bt.push (TorrentLoaded dl) *)

let rec fetch_metadata bt =
  let peers = Hashtbl.create 20 in
  let send id m = try let p, send = Hashtbl.find peers id in send (m p) with Not_found -> () in
  let rec loop m =
    Lwt_stream.next bt.chan >>= fun e ->
    match m, e with
    | None, AvailableMetadata (p, len) ->
        (* debug "%s offered %d bytes of metadata" (Peer.to_string p) len; *)
        (* FIXME *)
        let m = IncompleteMetadata.create bt.ih len in
        loop (Some m)
    | Some _, AvailableMetadata _ ->
        loop m

    | _, PeersReceived addrs ->
        (* debug "received %d peers" (List.length addrs); *)
        List.iter (fun addr -> bt.push (PeerMgr.add bt.peer_mgr addr)) addrs;
        loop m

    | _, ConnectPeer (addr, timeout) ->
        connect_to_peer bt.ih bt.push addr timeout;
        loop m

    | None, PeerConnected (mode, fd, exts, id) ->
        let p, send = welcome bt.push mode fd exts id in
        Hashtbl.replace peers id (p, send);
        loop m

    | Some m', PeerConnected (mode, fd, exts, id) ->
        let p, send = welcome bt.push mode fd exts id in
        IncompleteMetadata.iter_missing
          (fun i -> send @@ Peer.request_meta_piece p i) m';
        Hashtbl.replace peers id (p, send);
        loop m

    | _, PeerDisconnected id ->
        bt.push (PeerMgr.peer_disconnected bt.peer_mgr id);
        Hashtbl.remove peers id;
        loop m

    | _, ConnectFailed addr ->
        bt.push (PeerMgr.handshake_failed bt.peer_mgr addr);
        loop m

    | _, MetaRequested (id, i) ->
        send id (Peer.reject_metadata_request i);
        loop m

    | Some m', GotMetaPiece (p, i, s) ->
        (* debug "got metadata piece %d/%d from %s" i *)
        (*   (IncompleteMetadata.piece_count m) (Peer.to_string p); *)
        begin match IncompleteMetadata.add m' i s with
        | `Failed ->
            (* debug "metadata hash check failed; trying again"; *)
            loop None
        | `Verified raw ->
              (* debug "got full metadata"; *)
            let m' = Metadata.create (Bcode.decode raw) in
            Lwt.return m'
        | `More ->
            loop m
        end

    | None, GotMetaPiece _ ->
        loop m

    | _, GotPEX (p, added, dropped) ->
        (* debug "got pex from %s added %d dropped %d" (Peer.to_string p) *)
        (*   (List.length added) (List.length dropped); *)
        List.iter (fun (addr, _) -> bt.push (PeerMgr.add bt.peer_mgr addr)) added;
        loop m

    | _, DHTPort (p, i) ->
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
    | _, TorrentComplete
    | _, Unchoked _
    | _, Choked _
    | _, Interested _
    | _, NotInterested _
    | _, Have _
    | _, HaveBitfield _
    | _, RejectMetaPiece _
    | _, BlockRequested _
    | _, BlockReceived _
    | _, NoEvent ->
        loop m
  in
  loop None

let start bt =
  List.iter (fun tier -> Lwt.async (fun () -> announce ~info_hash:bt.ih tier bt.push bt.id)) bt.trackers;
  fetch_metadata bt >>= fun meta ->
  load_torrent bt meta >>= fun tor ->
  assert false

let listen_backlog = 5

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

let create mg =
  let chan, push = Lwt_stream.create () in
  let push x = push (Some x) in
  let id = SHA1.generate ~prefix:"OCAML" () in
  let ih = mg.Magnet.xt in
  let peer_mgr = PeerMgr.create () in
  Lwt.async (fun () -> start_server push);
  { id; ih; trackers = mg.Magnet.tr; chan;
    push; peer_mgr }
