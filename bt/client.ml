(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let max_num_peers = 40
let max_connections = 5
let listen_ports = [50000]
let max_requests = 5
let max_downloaders_unchoke = 4
let unchoking_frequency = 10
let optimistic_unchoke_iterations = 3
let rate_computation_iterations = 2

type event =
  | IncomingPeer of Tcp.socket * Addr.t
  | PeersReceived of Addr.t list
  | PeerEvent of Peer.t * Peer.event
  | GotMetadata of Meta.t
  | TorrentLoaded of int * int * Bits.t
  | PieceVerified of int
  | TorrentCompleted
  | Rechoke of int * int
  | Announce of Tracker.Tier.t * Tracker.event option

type stage =
  | NoMeta
  | PartialMeta of Meta.partial
  | Loading of Meta.t * Torrent.t
  | Leeching of Meta.t * Torrent.t
  | Seeding of Meta.t * Torrent.t

type t = {
  id : SHA1.t;
  ih : SHA1.t;
  mutable trackers : Tracker.Tier.t list;
  peers : (Addr.t, Peer.t) Hashtbl.t;
  connecting : (Addr.t, unit) Hashtbl.t;
  mutable saved : Addr.t list;
  chan : event Lwt_stream.t;
  push : event -> unit;
  mutable stage : stage;
  mutable port : int
}

let create mg =
  let chan, push = Lwt_stream.create () in
  let push x = push (Some x) in
  let trackers =
    List.map (fun tr ->
        let tier = Tracker.Tier.create () in
        Tracker.Tier.add_tracker tier tr;
        tier) mg.Magnet.tr
  in
  { id = SHA1.peer_id "OCTO";
    ih = mg.Magnet.xt;
    trackers;
    peers = Hashtbl.create 17;
    connecting = Hashtbl.create 17;
    saved = [];
    chan;
    push;
    stage = NoMeta;
    port = -1}

exception Cant_listen

let create_server handle =
  let sock = Tcp.create_socket () in
  let rec loop = function
    | [] ->
      raise Cant_listen
    | p :: ports ->
      try
        let stop = Tcp.listen sock p handle in
        Log.info "listening on port %d" p;
        p, stop
      with _ -> loop ports
  in
  loop listen_ports

let push_incoming_peer bt sock addr =
  bt.push (IncomingPeer (sock, addr))

let push_peers_received bt xs =
  bt.push (PeersReceived xs)

let push_peer_event bt p ev =
  bt.push (PeerEvent (p, ev))

let push_metadata bt info =
  bt.push (GotMetadata info)

let proto = "BitTorrent protocol"

let read_handshake sock =
  Tcp.read sock (49 + String.length proto) >|= fun hs ->
  bitmatch Bitstring.bitstring_of_string hs with
  | { 19 : 8;
      proto : 19 * 8 : string;
      extbits : 8 * 8 : string, bind (Bits.of_bin extbits);
      ih : 20 * 8 : string, bind (SHA1.from_bin ih);
      id : 20 * 8 : string, bind (SHA1.from_bin id) } ->
    (ih, id, extbits)

let extended_bits =
  let bits = Bits.create (8 * 8) in
  Bits.set bits Wire.lt_extension_bit;
  bits

let handshake_message id ih =
  BITSTRING
    { 19 : 8; proto : -1 : string;
      Bits.to_bin extended_bits : 8 * 8 : string;
      SHA1.to_bin ih : 20 * 8 : string;
      SHA1.to_bin id : 20 * 8 : string }

let know_peer bt addr =
  Hashtbl.mem bt.peers addr || Hashtbl.mem bt.connecting addr || List.mem addr bt.saved

let is_seeding bt =
  match bt.stage with
  | Seeding _ -> true
  | _ -> false

let need_more_peers bt =
  (Hashtbl.length bt.peers + Hashtbl.length bt.connecting < max_num_peers) &&
  not (is_seeding bt)
  
let peer_joined bt sock addr ih id exts =
  let p = Peer.create sock id in
  Peer.start p (push_peer_event bt p);
  if Bits.is_set exts Wire.lt_extension_bit then Peer.send_extended_handshake p;
  Hashtbl.add bt.peers addr p;
  match bt.stage with
  | Leeching (_, t)
  | Seeding (_, t) -> Peer.send_have_bitfield p (Torrent.have t)
  | _ -> ()

let rec peer_connection_failed bt addr =
  if need_more_peers bt && List.length bt.saved > 0 then begin
    let addr = List.hd bt.saved in
    bt.saved <- List.tl bt.saved;
    let doit () =
      let sock = Tcp.create_socket () in
      Tcp.connect sock addr >>= fun () ->
      Tcp.write_bitstring sock (handshake_message bt.id bt.ih) >>= fun () ->
      read_handshake sock >|= fun (ih, id, exts) ->
      Log.success "handshake successful (addr=%s,ih=%s,id=%s)"
        (Addr.to_string addr) (SHA1.to_hex_short ih) (SHA1.to_hex_short id);
      peer_joined bt sock addr ih id exts
    in
    Lwt.async (fun () -> try_connect bt addr doit)
  end

and try_connect bt addr f =
  Hashtbl.add bt.connecting addr ();
  let doit () = Lwt.finalize f (fun () -> Hashtbl.remove bt.connecting addr; Lwt.return ()) in
  Lwt.catch doit
    (fun e ->
       Log.error ~exn:e "try_connect";
       peer_connection_failed bt addr;
       Lwt.return ())

let peer_finished bt p =
  Log.info "peer disconnected (addr=%s)" (Addr.to_string (Peer.addr p));
  Hashtbl.remove bt.peers (Peer.addr p);
  if need_more_peers bt && List.length bt.saved > 0 then begin
    let addr = List.hd bt.saved in
    bt.saved <- List.tl bt.saved;
    Log.info "contacting saved peer (addr=%s)" (Addr.to_string addr);
    let doit () =
      let sock = Tcp.create_socket () in
      Tcp.connect sock addr >>= fun () ->
      Tcp.write_bitstring sock (handshake_message bt.id bt.ih) >>= fun () ->
      read_handshake sock >|= fun (ih, id, exts) ->
      Log.success "handshake successful (addr=%s,ih=%s,id=%s)"
        (Addr.to_string addr) (SHA1.to_hex_short ih) (SHA1.to_hex_short id);
      peer_joined bt sock addr ih id exts
    in
    Lwt.async (fun () -> try_connect bt addr doit)
  end

let handle_incoming_peer bt sock addr =
  if not (know_peer bt addr) then
    if need_more_peers bt then begin
      Log.info "contacting incoming peer (addr=%s)" (Addr.to_string addr);
      let doit () =
        read_handshake sock >>= fun (ih, id, exts) ->
        Tcp.write_bitstring sock (handshake_message bt.id ih) >|= fun () ->
        peer_joined bt sock addr ih id exts
      in
      Lwt.async (fun () -> try_connect bt addr doit)
    end else begin
      Log.warning "too many peers; saving incoming peer for later (addr=%s)" (Addr.to_string addr);
      bt.saved <- addr :: bt.saved;
      Lwt.async (fun () -> Tcp.close sock)
    end

let handle_received_peer bt addr =
  if not (know_peer bt addr) then
    if need_more_peers bt then begin
      Log.info "contacting outgoing peer (addr=%s)" (Addr.to_string addr);
      let doit () =
        let sock = Tcp.create_socket () in
        Tcp.connect sock addr >>= fun () ->
        Tcp.write_bitstring sock (handshake_message bt.id bt.ih) >>= fun () ->
        read_handshake sock >|= fun (ih, id, exts) ->
        Log.success "handshake successful (addr=%s,ih=%s,id=%s)"
          (Addr.to_string addr) (SHA1.to_hex_short ih) (SHA1.to_hex_short id);
        peer_joined bt sock addr ih id exts
      in
      Lwt.async (fun () -> try_connect bt addr doit)
    end
    else begin
      Log.warning "too many peers; saving outgoing peer for later (addr=%s)" (Addr.to_string addr);
      bt.saved <- addr :: bt.saved
    end
    
let unchoke_peers bt optimistic =
  let aux compare_peers =
    let peers = Hashtbl.fold (fun _ p l -> p :: l) bt.peers [] in
    let peers = List.sort compare_peers peers in
    let rec loop choked downloaders = function
      | [] -> choked
      | p :: peers ->
        if downloaders < max_downloaders_unchoke then
          if Peer.am_choking p then begin
            Peer.send_unchoke p;
            if Peer.peer_interested p then loop choked (downloaders + 1) peers
            else loop choked downloaders peers
          end else
            loop choked downloaders peers
        else begin
          loop (p :: choked) downloaders peers
        end
    in
    let choked = loop [] 0 peers in
    if List.length choked > 0 then
      let r = Random.int (List.length choked) in
      let i = ref 0 in
      List.iter (fun p ->
          if optimistic && !i = r then Peer.send_unchoke p else Peer.send_choke p;
          incr i) choked
  in
  match bt.stage with
  | Leeching _ ->
    aux (fun a b -> compare (Peer.download_rate b) (Peer.download_rate a))
  | Seeding _ ->
    aux (fun a b -> compare (Peer.upload_rate b) (Peer.upload_rate a))
  | _ ->
    ()
          
let info_piece_size = 16 * 1024

let roundup n r =
  (n + r - 1) / r * r

let has_metadata bt =
  match bt.stage with
  | Loading _
  | Leeching _
  | Seeding _ -> true
  | _ -> false

let has_partial_metadata bt =
  match bt.stage with
  | NoMeta -> false
  | PartialMeta _
  | Loading _
  | Leeching _
  | Seeding _ -> true

let request_meta_piece bt (p : Peer.t) =
  match bt.stage with
  | PartialMeta meta ->
    begin match Meta.pick_missing meta with
    | None -> ()
    | Some i -> Peer.request_meta_piece p i
    end
  | _ ->
    ()

let handle_available_metadata bt p len =
  let npieces = roundup len info_piece_size / info_piece_size in
  Log.success "metadata available (len=%d,npieces=%d)" len npieces;
  match bt.stage with
  | NoMeta ->
    bt.stage <- PartialMeta (Meta.create_partial bt.ih len);
    request_meta_piece bt p
  | PartialMeta m ->
    if Meta.partial_length m = len then
      request_meta_piece bt p
    else
      Log.warning "metadata size mismatch (expected=%d,received=%d)"
        (Meta.partial_length m) len
  | _ ->
    ()

let request_block bt p =
  match bt.stage with
  | Leeching (_, t) ->
    begin match Torrent.request_block t (Peer.has_piece p) with
    | None ->
      Peer.send_not_interested p
    | Some pc ->
      Peer.send_request p pc
    end
  | _ -> ()
  
let handle_peer_event bt p = function
  | Peer.Finished reqs ->
    peer_finished bt p;
    begin match bt.stage with
    | Leeching (_, dl) ->
      Torrent.lost_bitfield dl (Peer.have p)
    | _ -> ()
    end
  | Peer.AvailableMetadata len ->
    handle_available_metadata bt p len
  | Peer.Choked reqs ->
    begin match bt.stage with
    | Leeching (_, t) -> List.iter (Torrent.lost_request t) reqs
    | _ -> ()
    end
  | Peer.Unchoked ->
    Log.info "peer just unchoked us (addr=%s)" (Addr.to_string (Peer.addr p));
    for i = 1 to max_requests do
      request_block bt p
    done
  | Peer.Have i ->
    begin match bt.stage with
    | Leeching (_, dl) ->
      if Torrent.got_have dl i then Peer.send_interested p
    | _ -> ()
    end
  | Peer.HaveBitfield b ->
    begin match bt.stage with
    | Leeching (_, dl) ->
      if Torrent.got_bitfield dl b then Peer.send_interested p
    | _ -> ()
    end
  | Peer.MetaRequested i ->
    begin match bt.stage with
    | NoMeta
    | PartialMeta _ ->
      Peer.send_reject_meta p i
    | Loading (meta, _)
    | Leeching (meta, _)
    | Seeding (meta, _) ->
      Peer.send_meta_piece p i (Meta.length meta, Meta.get_piece meta i)
    end
  | Peer.GotMetaPiece (i, s) ->
    begin match bt.stage with
    | PartialMeta meta ->
      Log.success "got meta piece (i=%d)" i;
      if Meta.add_piece meta i s then
        match Meta.verify meta with
        | Some meta ->
          Log.success "got complete metadata";
          push_metadata bt meta
        | None ->
          Log.error "metadata hash check failed";
          bt.stage <- NoMeta
      else
        request_meta_piece bt p
    | _ ->
      ()
    end
  | Peer.RejectMetaPiece i ->
    Log.warning "meta piece rejected (i=%d)" i;
    request_meta_piece bt p
  | Peer.BlockRequested (idx, off, len) ->
    begin match bt.stage with
    | Leeching (_, dl)
    | Seeding (_, dl) ->
      if Torrent.has_piece dl idx then begin
        let aux _ =
          Torrent.get_block dl idx off len >|= Peer.send_block p idx off
        in
        Lwt.async aux
      end
    | _ ->
      ()
    end
  | Peer.BlockReceived (idx, off, s) ->
    begin match bt.stage with
    | Leeching (meta, t) ->
      Log.success "received block (idx=%d,off=%d,len=%d)" idx off (String.length s);
      let aux () =
        Torrent.got_block t idx off s >|= function
        | `Verified ->
          bt.push (PieceVerified idx);
          if Torrent.is_complete t then bt.push TorrentCompleted
        | `Failed
        | `Continue -> ()
      in
      Lwt.async aux;
      request_block bt p
    | _ ->
      ()
    end
  | Peer.Interested ->
    ()
  | Peer.NotInterested ->
    ()
  | Peer.Port _ ->
    ()

let reset_peer_rates bt =
  Hashtbl.iter (fun _ p -> Peer.reset_rates p) bt.peers

let print_info bt =
  match bt.stage with
  | Leeching (_, t)
  | Seeding (_, t) ->
    let dl, ul =
      Hashtbl.fold
        (fun _ p (dl, ul) -> (dl +. Peer.download_rate p, ul +. Peer.upload_rate p))
        bt.peers (0.0, 0.0)
    in
    let eta =
      let left = Torrent.amount_left t in
      if dl = 0.0 then "Inf"
      else
        let eta = Int64.to_float left /. dl in
        let tm = Unix.gmtime eta in
        if tm.Unix.tm_mday > 1 || tm.Unix.tm_mon > 0 || tm.Unix.tm_year > 70 then "More than a day"
        else
          Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    in

    Printf.eprintf "Progress: %d/%d (%d%%) Peers: %d Downloaded: %s (%s/s) Uploaded: %s (%s/s) ETA: %s\n%!"
      (Bits.count (Torrent.have t))
      (Bits.length (Torrent.have t))
      (truncate (100.0 *. (float (Bits.count (Torrent.have t))) /. (float (Bits.length (Torrent.have t)))))
      (Hashtbl.length bt.peers)
      (Util.string_of_file_size (Torrent.down t))
      (Util.string_of_file_size (Int64.of_float dl))
      (Util.string_of_file_size (Torrent.up t))
      (Util.string_of_file_size (Int64.of_float ul))
      eta
  | _ ->
    ()

let handle_event bt = function
  | IncomingPeer (sock, addr) ->
    handle_incoming_peer bt sock addr
  | PeersReceived addrs ->
    Log.success "received %d peers" (List.length addrs);
    let aux () =
      let rec loop = function
        | [] -> Lwt.return ()
        | addr :: addrs ->
          handle_received_peer bt addr;
          Lwt_unix.sleep 0.1 >>= fun () -> loop addrs
      in
      loop addrs
    in
    Lwt.async aux
  | PeerEvent (p, e) ->
    handle_peer_event bt p e
  | GotMetadata meta ->
    let t = Torrent.create meta in
    bt.stage <- Loading (meta, t);
    let aux () =
      Torrent.update t >|= fun (good, tot, bits) -> bt.push (TorrentLoaded (good, tot, bits))
    in
    Lwt.async aux
  | TorrentLoaded (good, total, bits) ->
    begin match bt.stage with
    | Loading (meta, t) ->
      Log.success "torrent loaded (good=%d,total=%d)" good total;
      bt.stage <- if Torrent.is_complete t then Seeding (meta, t) else Leeching (meta, t);
      let wakeup_peer _ p =
        Peer.send_have_bitfield p bits;
        if Torrent.got_bitfield t (Peer.have p) then begin
          Log.success "we should be interested in %s" (Addr.to_string (Peer.addr p));
          Peer.send_interested p;
          if not (Peer.peer_choking p) then
            for i = 1 to max_requests do request_block bt p done
        end else
          Peer.send_not_interested p
      in
      Hashtbl.iter wakeup_peer bt.peers;
      bt.push (Rechoke (1, 1))
    | _ ->
      ()
    end
  | PieceVerified i ->
    Log.success "piece verified and written to disk (idx=%d)" i
  | TorrentCompleted ->
    Log.success "torrent completed!";
    begin match bt.stage with
    | Leeching (meta, t) -> bt.stage <- Seeding (meta, t)
    | _ -> ()
    end
  | Rechoke (optimistic, rateiter) ->
    Log.info "rechoking (optimistic=%d,rateiter=%d)" optimistic rateiter;
    let optimistic = if optimistic = 0 then optimistic_unchoke_iterations else optimistic - 1 in
    let rateiter = if rateiter = 0 then rate_computation_iterations else rateiter - 1 in
    unchoke_peers bt (optimistic = 0);
    print_info bt;
    if rateiter = 0 then reset_peer_rates bt;
    Lwt.async
      (fun () ->
         Lwt_unix.sleep (float unchoking_frequency) >|= fun () ->
         bt.push (Rechoke (optimistic, rateiter)))
  | Announce (tier, event) ->
    let doit () =
      Tracker.Tier.query tier ~ih:bt.ih ?up:None ?down:None ?left:None ?event ~port:bt.port ~id:bt.id >>= fun resp ->
      Log.success "announce on %s successful, reannouncing in %d seconds"
        (Tracker.Tier.show tier) resp.Tracker.interval;
      push_peers_received bt resp.Tracker.peers;
      Lwt_unix.sleep (float resp.Tracker.interval) >|= fun () ->
      bt.push (Announce (tier, None))
    in
    let safe_doit () =
      Lwt.catch doit (fun e -> Log.error ~exn:e "announce failure"; Lwt.return ())
    in
    Lwt.async safe_doit

let event_loop bt =
  let rec loop () =
    Lwt_stream.next bt.chan >|= handle_event bt >>= loop
  in
  loop ()

let start bt =
  let port, _ = create_server (push_incoming_peer bt) in
  bt.port <- port;
  Log.info "starting";
  List.iter (fun tier -> bt.push (Announce (tier, Some Tracker.STARTED))) bt.trackers;
  event_loop bt
