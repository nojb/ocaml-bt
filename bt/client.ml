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

let max_num_peers = 20
let max_connections = 5
let listen_ports = [50000]
let max_requests = 5
let max_downloaders_unchoke = 4
let unchoking_frequency = 10
let optimistic_unchoke_iterations = 3
let rate_computation_iterations = 2
  
(* module FH : sig *)
(*   type t *)
(*   val create : unit -> t *)
(*   val add : t -> int -> unit *)
(*   val add_bitfield : t -> Bits.t -> unit *)
(*   val iter : t -> (int -> int -> unit) -> unit *)
(*   val to_array : t -> int -> int array *)
(* end = struct *)
(*   type t = (int, int) Hashtbl.t *)
(*   let create () = Hashtbl.create 3 *)
(*   let add h i = *)
(*     let n = try Hashtbl.find h i with Not_found -> 0 in *)
(*     Hashtbl.replace h i (n+1) *)
(*   let add_bitfield h b = *)
(*     for i = 0 to Bits.length b - 1 do *)
(*       if Bits.is_set b i then add h i *)
(*     done *)
(*   let iter h f = *)
(*     Hashtbl.iter (fun i n -> if n > 0 then f i n) h *)
(*   let to_array h n = *)
(*     Array.init n (fun i -> try Hashtbl.find h i with Not_found -> 0) *)
(* end *)

type event =
  | IncomingPeer of IO.socket * Addr.t
  | PeersReceived of Addr.t list
  | PeerEvent of Peer.t * Peer.event
  | GotMetadata of Metadata.t
  | TorrentLoaded of Torrent.t
  | PieceVerified of int
  | TorrentCompleted
  | Announce of Tracker.Tier.t * Tracker.event option

type stage =
  | NoMeta
  | PartialMeta of IncompleteMetadata.t
  | Loading of Metadata.t
  | Leeching of Metadata.t * Torrent.t
  | Seeding of Metadata.t * Torrent.t

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

(* let create_server handle = *)
(*   let sock = Tcp.create_socket () in *)
(*   let rec loop = function *)
(*     | [] -> *)
(*       raise Cant_listen *)
(*     | p :: ports -> *)
(*       try *)
(*         let stop = Tcp.listen sock p handle in *)
(*         Log.info "listening on port %d" p; *)
(*         p, stop *)
(*       with _ -> loop ports *)
(*   in *)
(*   loop listen_ports *)

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
  sock#read_string (49 + String.length proto) >|= fun hs ->
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

let get_next_requests bt p n =
  match bt.stage with
  | Leeching (_, t) ->
    if not (Peer.peer_choking p) then Torrent.get_next_requests t p n
    else []
  | _ -> []

let get_next_metadata_request bt p () =
  match bt.stage with
  | PartialMeta m ->
    IncompleteMetadata.get_next_metadata_request m
  | _ ->
    None

let (!!) = Lazy.force

let peer_joined bt sock addr ih id exts =
  let rec p = lazy (Peer.create sock addr id (fun e -> push_peer_event bt !!p e)) in
  Peer.start !!p (get_next_requests bt !!p) (get_next_metadata_request bt !!p);
  if Bits.is_set exts Wire.lt_extension_bit then Peer.send_extended_handshake !!p;
  Hashtbl.add bt.peers addr !!p;
  match bt.stage with
  | Leeching (_, t)
  | Seeding (_, t) -> Peer.send_have_bitfield !!p (Torrent.have t)
  | _ -> ()

let rec connect_peer ?(retry = false) bt addr =
  IO.connect addr >>= fun sock ->
  let hs = Handshake.create ~incoming:false ~id:bt.id ~ih:bt.ih in
  Handshake.run hs (if retry then false else true) sock >>= function
  | Handshake.Success (id, exts, sock) ->
    Log.success "handshake successful (addr=%s,ih=%s,id=%s)"
      (Addr.to_string addr) (SHA1.to_hex_short bt.ih) (SHA1.to_hex_short id);
    peer_joined bt sock addr bt.ih id exts;
    Lwt.return ()
  | Handshake.Retry ->
    Log.info "encrypted handshake failed; retrying with plain...";
    connect_peer ~retry:true bt addr
  | _ ->
    Lwt.fail (Failure "error")

and try_connect bt addr f =
  Hashtbl.add bt.connecting addr ();
  let doit () = Lwt.finalize f (fun () -> Hashtbl.remove bt.connecting addr; Lwt.return ()) in
  Lwt.catch doit
    (fun e ->
       Log.error ~exn:e "try_connect";
       Lwt.return ())

let peer_finished bt p =
  Log.info "peer disconnected (addr=%s)" (Addr.to_string (Peer.addr p));
  Hashtbl.remove bt.peers (Peer.addr p)

let handle_incoming_peer bt sock addr =
  if not (know_peer bt addr) then
    if need_more_peers bt then begin
      Log.info "contacting incoming peer (addr=%s)" (Addr.to_string addr);
      let doit () =
        read_handshake sock >>= fun (ih, id, exts) ->
        sock#write_bitstring (handshake_message bt.id ih) >|= fun () ->
        peer_joined bt sock addr ih id exts
      in
      Lwt.async (fun () -> try_connect bt addr doit)
    end else begin
      Log.warning "too many peers; saving incoming peer for later (addr=%s)" (Addr.to_string addr);
      bt.saved <- addr :: bt.saved;
      Lwt.async (fun () -> sock#close)
    end

let handle_received_peer bt addr =
  if not (know_peer bt addr) then begin
    Log.warning "saving outgoing peer for later (addr=%s)" (Addr.to_string addr);
    bt.saved <- addr :: bt.saved
  end

let reconnect_pulse_delay = 0.5

let rec reconnect_pulse bt =
  while need_more_peers bt && List.length bt.saved > 0 do
    let addr = List.hd bt.saved in
    bt.saved <- List.tl bt.saved;
    Lwt.async (fun () -> try_connect bt addr (fun () -> connect_peer bt addr))
  done;
  Lwt_unix.sleep reconnect_pulse_delay >>= fun () -> reconnect_pulse bt

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

let reset_peer_rates bt =
  Hashtbl.iter (fun _ p -> Peer.reset_rates p) bt.peers

let rechoke_downloads bt =
  match bt.stage with
  | Leeching (_, t) ->
    let h = Torrent.have t in
    let doit p =
      let rec loop i =
        if i >= Bits.length h then false
        else if not (Bits.is_set h i) && Peer.has_piece p i then true else loop (i+1)
      in
      if loop 0 then Peer.send_interested p else Peer.send_not_interested p
    in
    Hashtbl.iter (fun _ p -> doit p) bt.peers
  | _ ->
    ()

let rec rechoke_pulse bt optimistic rateiter =
  Log.info "rechoking (optimistic=%d,rateiter=%d)" optimistic rateiter;
  let optimistic = if optimistic = 0 then optimistic_unchoke_iterations else optimistic - 1 in
  let rateiter = if rateiter = 0 then rate_computation_iterations else rateiter - 1 in
  unchoke_peers bt (optimistic = 0);
  rechoke_downloads bt;
  print_info bt;
  if rateiter = 0 then reset_peer_rates bt;
  Lwt_unix.sleep (float unchoking_frequency) >>= fun () ->
  rechoke_pulse bt optimistic rateiter
          
let info_piece_size = 16 * 1024

let roundup n r =
  (n + r - 1) / r * r

(* let has_metadata bt = *)
(*   match bt.stage with *)
(*   | Loading _ *)
(*   | Leeching _ *)
(*   | Seeding _ -> true *)
(*   | _ -> false *)

(* let has_partial_metadata bt = *)
(*   match bt.stage with *)
(*   | NoMeta _ -> false *)
(*   | PartialMeta _ *)
(*   | Loading _ *)
(*   | Leeching _ *)
(*   | Seeding _ -> true *)

let handle_available_metadata bt p len =
  let npieces = roundup len info_piece_size / info_piece_size in
  Log.success "metadata available (len=%d,npieces=%d)" len npieces;
  match bt.stage with
  | NoMeta ->
    bt.stage <- PartialMeta (IncompleteMetadata.create bt.ih len)
  | _ ->
    ()

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
  | Peer.Have i ->
    begin match bt.stage with
    | Leeching (_, dl) ->
      Torrent.got_have dl i
    | _ -> ()
    end
  | Peer.HaveBitfield b ->
    begin match bt.stage with
    | Leeching (_, dl) ->
      Torrent.got_bitfield dl b
    | _ -> ()
    end
  | Peer.MetaRequested i ->
    begin match bt.stage with
    | NoMeta
    | PartialMeta _ ->
      Peer.send_reject_meta p i
    | Loading meta
    | Leeching (meta, _)
    | Seeding (meta, _) ->
      Peer.send_meta_piece p i (Metadata.length meta, Metadata.get_piece meta i)
    end
  | Peer.GotMetaPiece (i, s) ->
    begin match bt.stage with
    | PartialMeta meta ->
      begin Log.success "got meta piece (i=%d)" i;
      if IncompleteMetadata.add_piece meta i s then
        match IncompleteMetadata.verify meta with
        | Some meta ->
          Log.success "got complete metadata";
          push_metadata bt (Metadata.create (Bcode.decode meta))
        | None ->
          Log.error "metadata hash check failed";
          bt.stage <- NoMeta
      end
    | _ ->
      ()
    end
  | Peer.RejectMetaPiece i ->
    Log.warning "meta piece rejected (i=%d)" i
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
      Log.success "received block (idx=%d,off=%d,len=%d) from %s (%s/s)"
        idx off (String.length s) (Addr.to_string (Peer.addr p))
        (Util.string_of_file_size (Int64.of_float (Peer.download_rate p)));
      let aux () =
        Torrent.got_block t idx off s >|= function
        | `Verified ->
          bt.push (PieceVerified idx);
          if Torrent.is_complete t then bt.push TorrentCompleted
        | `Failed
        | `Continue -> ()
      in
      Lwt.async aux
    | _ ->
      ()
    end
  | Peer.Port _ ->
    ()

let handle_event bt = function
  | IncomingPeer (sock, addr) ->
    handle_incoming_peer bt sock addr
  | PeersReceived addrs ->
    Log.success "received %d peers" (List.length addrs);
    List.iter (handle_received_peer bt) addrs
  | PeerEvent (p, e) ->
    handle_peer_event bt p e
  | GotMetadata meta ->
    bt.stage <- Loading meta;
    let aux () =
      Torrent.create meta >|= fun dl -> bt.push (TorrentLoaded dl)
    in
    Lwt.async aux
  | TorrentLoaded dl ->
    begin match bt.stage with
    | Loading meta ->
      Log.success "torrent loaded (good=%d,total=%d)"
        (Torrent.numgot dl) (Metadata.piece_count meta - Torrent.numgot dl);
      bt.stage <- if Torrent.is_complete dl then Seeding (meta, dl) else Leeching (meta, dl);
      let wakeup_peer _ p =
        Torrent.got_bitfield dl (Peer.have p);
        Peer.send_have_bitfield p (Torrent.have dl)
      in
      Hashtbl.iter wakeup_peer bt.peers
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
  (* let port, _ = create_server (push_incoming_peer bt) in *)
  let port = 5000 in
  bt.port <- port;
  Log.info "starting";
  List.iter (fun tier -> bt.push (Announce (tier, Some Tracker.STARTED))) bt.trackers;
  Lwt.async (fun () -> reconnect_pulse bt);
  Lwt.async (fun () -> rechoke_pulse bt 1 1);
  event_loop bt
