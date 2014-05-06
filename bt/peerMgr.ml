let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type meta_info =
  | HasMeta of Metadata.t * (Peer.t -> Peer.get_block_func)
  | NoMeta of (Peer.t -> Peer.get_metadata_func)

(* type 'a metadata = *)
(*   | HasMetaMeta : Metadata.t -> Peer.has_meta metadata *)
(*   | NoMetaMeta : Peer.no_meta metadata *)
              
and t = {
  id : SHA1.t;
  ih : SHA1.t;
  peers : (Addr.t, Peer.t) Hashtbl.t;
  connecting : (Addr.t, unit) Hashtbl.t;
  mutable saved : Addr.t list;
  handle_peer_event : Peer.t -> Peer.event_callback;
  mutable info : meta_info
  (* request : request_func; *)
  (* meta : Peer.metadata *)
  (* get_next_requests : Peer.t -> int -> (int * int * int) list; *)
  (* get_next_metadata_request : Peer.t -> unit -> int option *)
}

let handle_peer_event bt p e =
  bt.handle_peer_event p e

let create_has_meta id ih h m request =
  { id; ih; peers = Hashtbl.create 3;
    connecting = Hashtbl.create 3;
    saved = []; handle_peer_event = h;
    info = HasMeta (m, request) }

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
  Hashtbl.length bt.peers + Hashtbl.length bt.connecting < max_num_peers
(* && not (is_seeding bt) *)

let (!!) = Lazy.force

(* let request_func f p = *)
(*   match f with *)
(*   | HasMeta (_, f) -> q (fun n -> f !!p n) *)
(*   | NoMeta f -> Peer.NoMetaReq (fun () -> f !!p ()) *)
 
let peer_joined bt sock addr ih id exts =
  (* let p = *)
  let p =
    match bt.info with
    | HasMeta (m, r) ->
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
  if Bits.is_set exts Wire.lt_extension_bit then Peer.send_extended_handshake !!p;
  Hashtbl.add bt.peers addr !!p
(* FIXME *)
  (* match bt.stage with *)
  (* | Leeching (_, t) *)
  (* | Seeding (_, t) -> Peer.send_have_bitfield !!p (Torrent.have t) *)
  (* | _ -> () *)

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

let got_metadata pm m get_next_requests =
  Hashtbl.iter (fun a p -> Peer.got_metadata p m (get_next_requests p)) pm.peers;
  pm.info <- HasMeta (m, get_next_requests);
  (* { id = pm.id; *)
  (*   ih = pm.ih; *)
  (*   peers; *)
  (*   connecting = pm.connecting; *)
  (*   saved = pm.saved; *)
  (*   handle_peer_event = handle_peer_event; (\* pm.handle_peer_event; *\) *)
  (*   info = HasMeta (m, handle) } *)
    (* meta = Peer.HasMetaMeta m } *)
  (* get_next_requests : Peer.t -> int -> (int * int * int) list; *)
  (* get_next_metadata_request : Peer.t -> unit -> int option *)
