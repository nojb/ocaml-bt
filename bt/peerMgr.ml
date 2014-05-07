let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type meta_info =
  | HasMeta of Metadata.t * Torrent.t * (Peer.t -> Peer.get_block_func)
  | NoMeta of (Peer.t -> Peer.get_metadata_func)

type t = {
  id : SHA1.t;
  ih : SHA1.t;
  peers : (Addr.t, Peer.t) Hashtbl.t;
  connecting : (Addr.t, unit) Hashtbl.t;
  mutable saved : Addr.t list;
  handle_peer_event : Peer.t -> Peer.event_callback;
  mutable info : meta_info
}

let handle_peer_event bt p e =
  bt.handle_peer_event p e

let create_has_meta id ih h m tor request =
  { id; ih; peers = Hashtbl.create 3;
    connecting = Hashtbl.create 3;
    saved = []; handle_peer_event = h;
    info = HasMeta (m, tor, request) }

let create_no_meta id ih h get_next_metadata_request =
  { id; ih; peers = Hashtbl.create 3;
    connecting = Hashtbl.create 3;
    saved = []; handle_peer_event = h;
    info = NoMeta get_next_metadata_request }

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

let max_num_peers = 20

let need_more_peers bt =
  (Hashtbl.length bt.peers + Hashtbl.length bt.connecting < max_num_peers)
(* && not (is_seeding bt) *)

let (!!) = Lazy.force

let peer_joined bt sock addr ih id exts =
  let p =
    match bt.info with
    | HasMeta (m, _, r) ->
      let rec p =
        lazy (Peer.create_has_meta sock addr id
                (fun e -> handle_peer_event bt !!p e) m (fun n -> r !!p n))
      in
      p
    | NoMeta r ->
      let rec p =
        lazy (Peer.create_no_meta sock addr id
                (fun e -> handle_peer_event bt !!p e) (fun () -> r !!p ()))
      in
      p
  in
  Peer.start !!p;
  Hashtbl.add bt.peers addr !!p;
  if Bits.is_set exts Wire.lt_extension_bit then Peer.send_extended_handshake !!p;
  match bt.info with
  | HasMeta (_, tor, _) -> Peer.send_have_bitfield !!p (Torrent.have tor)
  | NoMeta _ -> ()

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

let fold_peers f pm x =
  Hashtbl.fold (fun _ p l -> f p l) pm.peers x

let start pm =
  Lwt.async (fun () -> reconnect_pulse pm)

let iter_peers f pm =
  Hashtbl.iter (fun _ p -> f p) pm.peers

let torrent_loaded pm m tor get_next_requests =
  Log.debug "torrent_loaded (have %d pieces)" (Torrent.numgot tor);
  Hashtbl.iter (fun _ p -> Peer.got_metadata p m (get_next_requests p)) pm.peers;
  let b = Torrent.have tor in
  Hashtbl.iter begin fun _ p ->
    for i = 0 to Bits.length b - 1 do
      if Bits.is_set b i then Peer.send_have p i
    done
  end pm.peers;
  pm.info <- HasMeta (m, tor, get_next_requests)

let close_peer pm p =
  (* close and ban *)
  ()

let is_seed pm =
  match pm.info with
  | HasMeta (_, tor, _) -> Torrent.is_complete tor
  | NoMeta _ -> false

let peer_count pm =
  Hashtbl.length pm.peers

(* let is_bad_peer pm p = *)
(*   if is_seed pm && Peer.is_seed p then true (\* FIXME pex *\) else begin *)
(*     let relax_strictness_if_fewer_than_n = float pm.max_peer_count *. 0.9 + 0.5 in *)
(*     let strictness = *)
(*       if peer_count pm >= relax_strictness_if_fewer_than_n then 1.0 *)
(*       else (float (peer_count pm)) /. relax_strictness_if_fewer_than_n *)
(*     in *)
(*     let limit = hi - ((hi - lo) *. strictness) in *)
(*     let idle_time = Lwt_unix.time () -. max (....) in *)
(*     if idle_time > limit then *)
(*       true *)
(*     else *)
(*       false *)
(*   end *)

let is_bad_peer pm p =
  false

let close_bad_peers pm =
  iter_peers (fun p -> if is_bad_peer pm p then close_peer pm p) pm

let max_bad_pieces_per_peer = 5

let got_bad_piece pm i =
  iter_peers (fun p ->
      if Peer.worked_on_piece p i then
        if Peer.strike p >= max_bad_pieces_per_peer then
          close_peer pm p) pm

let got_piece pm i =
  iter_peers (fun p -> Peer.send_have p i) pm
