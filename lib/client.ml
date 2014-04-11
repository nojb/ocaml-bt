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

module H = Word160

let max_num_peers = 60
let max_connections = 5
let listen_ports = [50000]

(* let _ = Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Debug *)

(* let log ?exn fmt = *)
(*   Printf.ksprintf (fun msg -> Lwt_log.ign_debug_f ?exn "[client] %s" msg) fmt *)

let error ?exn fmt = Log.error ~sec:"client" ?exn fmt
let warning ?exn fmt = Log.warning ~sec:"client" ?exn fmt
let success ?exn fmt = Log.success ~sec:"client" ?exn fmt
let info ?exn fmt = Log.info ~sec:"client" ?exn fmt

type event =
  | IncomingPeer of Tcp.socket * Addr.t
  | PeersReceived of Addr.t list
  | PeerEvent of Peer.t * Peer.event

type metadata_state =
  | Nothing
  | Partial of string * bool array
  | Complete of Info.t

type t = {
  id : H.t;
  ih : H.t;
  trackers : Tracker.t list list;
  peers : (Addr.t, Peer.t) Hashtbl.t;
  connecting : (Addr.t, unit) Hashtbl.t;
  chan : event Lwt_stream.t;
  push : event -> unit;
  mutable metadata : metadata_state
}

let create mg =
  (* let mg = Magnet.of_string s in *)
  let chan, push = Lwt_stream.create () in
  let push x = push (Some x) in
  { id = H.peer_id "OCTO";
    ih = mg.Magnet.xt;
    trackers = List.map (fun tr -> [Tracker.create tr]) mg.Magnet.tr;
    peers = Hashtbl.create 17;
    connecting = Hashtbl.create 17;
    chan;
    push;
    metadata = Nothing }

exception Cant_listen

let create_server handle =
  let sock = Tcp.create_socket () in
  let rec loop = function
    | [] ->
      raise Cant_listen
    | p :: ports ->
      try
        let stop = Tcp.listen sock p handle in
        info "listening on port %d" p;
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

let proto = "BitTorrent protocol"

let read_handshake sock =
  let get_handshake =
    let open Get in
    char (String.length proto |> Char.chr) >>
    string proto >>
    string_of_length 8 >|= Bits.of_bin >>= fun extbits ->
    string_of_length 20 >|= Word160.from_bin >>= fun ih ->
    string_of_length 20 >|= Word160.from_bin >>= fun id ->
    return (ih, id, extbits)
  in
  Tcp.read sock (49 + String.length proto) >|= Get.run get_handshake

let extended_bits =
  let bits = Bits.create (8 * 8) in
  Bits.set bits Wire.lt_extension_bit;
  bits

let handshake_message id ih =
  Printf.sprintf "%c%s%s%s%s"
    (String.length proto |> Char.chr) proto
    (Bits.to_bin extended_bits)
    (Word160.to_bin ih)
    (Word160.to_bin id)

let add_peer bt sock addr ih id exts =
  if Hashtbl.length bt.peers < max_num_peers then begin
    let p = Peer.create sock id in
    Peer.start p (push_peer_event bt p);
    if Bits.is_set exts Wire.lt_extension_bit then Peer.send_extended_handshake p;
    Hashtbl.add bt.peers addr p
  end else begin
    warning "too many peers; rejecting new peer (addr=%s)" (Addr.to_string addr);
    Tcp.close sock |> ignore
  end

let handle_incoming_peer bt sock addr =
  info "incoming peer (addr=%s,present=%b)" (Addr.to_string addr) (Hashtbl.mem bt.peers addr);
  if not (Hashtbl.mem bt.peers addr) then begin
    Lwt.catch
      (fun () ->
        read_handshake sock >>= fun (ih, id, exts) ->
        Tcp.write sock (handshake_message bt.id ih) >|= fun () ->
        add_peer bt sock addr ih id exts)
      (fun e ->
         error ~exn:e "incoming peer handshake"; Lwt.return ()) |> ignore
  end else
    Tcp.close sock |> ignore
 
let handle_received_peer bt addr =
  info "received peer (addr=%s,present=%b,connecting=%b)"
    (Addr.to_string addr) (Hashtbl.mem bt.peers addr) (Hashtbl.mem bt.connecting addr);
  if not (Hashtbl.mem bt.peers addr || Hashtbl.mem bt.connecting addr) then begin
    Hashtbl.add bt.connecting addr ();
    Lwt.finalize
      (fun () ->
         let sock = Tcp.create_socket () in
         Tcp.connect sock addr >>= fun () ->
         Tcp.write sock (handshake_message bt.id bt.ih) >>= fun () ->
         read_handshake sock >>= fun (ih, id, exts) ->
         success "handshake successful (addr=%s,ih=%s,id=%s)"
           (Addr.to_string addr) (Word160.to_hex_short ih) (Word160.to_hex_short id);
         add_peer bt sock addr ih id exts;
         Lwt.return ())
      (fun () ->
         Hashtbl.remove bt.connecting addr; Lwt.return ())
  end else begin
    Lwt.return ()
  end

let info_piece_size = 16 * 1024

let roundup n r =
  (n + r - 1) / r * r

let has_metadata bt =
  match bt.metadata with
  | Nothing
  | Partial _ -> false
  | Complete _ -> true

let has_partial_metadata bt =
  match bt.metadata with
  | Nothing -> false
  | Partial _
  | Complete _ -> true

let handle_peer_event bt p ev =
  match ev with
  | `Finished ->
    Hashtbl.remove bt.peers (Peer.addr p)
  | `GotMetadata len ->
    if not (has_partial_metadata bt) then begin
      let buf = String.create len in
      let npieces = roundup len info_piece_size / info_piece_size in
      let have = Array.create npieces false in
      bt.metadata <- Partial (buf, have);
      success "received metadata info (len=%d)" len
    end;
    begin match bt.metadata with
      | Nothing -> assert false
      | Partial (buf, have) ->
        let rec loop i =
          if i >= Array.length have || has_metadata bt then
            Lwt.return ()
          else if not have.(i) then begin
            Peer.request_meta_piece p i >>= fun s ->
            if not have.(i) then begin
              have.(i) <- true;
              String.blit s 0 buf (i * info_piece_size) (String.length s)
            end;
            loop (i+1)
          end else
            loop (i+1)
        in
        Lwt.catch
          (fun () ->
             loop 0 >|= fun () ->
             if not (has_metadata bt) then
               let sha = Word160.digest_of_string buf in
               if sha = bt.ih then begin
                 bt.metadata <- Complete (Info.create (Get.run Bcode.bdecode buf));
                 success "got complete metadata"
               end else begin
                 error "bad metadata data; retrying";
                 bt.metadata <- Nothing
               end)
          (fun e -> Lwt.return ()) |> ignore
      | Complete _ ->
        ()
    end
  | _ ->
    ()

(* let max_concurrent_connections_try = 5 *)
    
let event_loop bt =
  let rec loop () =
    Lwt_stream.next bt.chan >|= begin function
    | IncomingPeer (sock, addr) ->
      handle_incoming_peer bt sock addr
    | PeersReceived addrs ->
      success "received %d peers" (List.length addrs);
      let _ = Lwt_list.iter_p (fun addr ->
          Lwt.catch
            (fun () -> handle_received_peer bt addr)
            (fun e ->
               error ~exn:e "error while connecting (addr=%s)" (Addr.to_string addr);
               Lwt.return ())) addrs in
      ()
      (* let addrs = ref addrs in *)
      (* let rec loop () = *)
      (*   if List.length !addrs = 0 then *)
      (*     Lwt.return () *)
      (*   else *)
      (*     let addr = List.hd !addrs in *)
      (*     addrs := List.tl !addrs; *)
      (*     Lwt.catch *)
      (*       (fun () -> handle_received_peer bt addr) *)
      (*       (fun e -> *)
      (*          error ~exn:e "error while connecting (addr=%s)" (Addr.to_string addr); *)
      (*          Lwt.return ()) *)
      (*     >>= loop *)
      (* in *)
      (* let rec enum i = *)
      (*   if i >= max_concurrent_connections_try then [] *)
      (*   else loop () :: enum (i+1) *)
      (* in *)
      (* success "received %d peers; trying to contact (max=%d)" *)
      (*   (List.length !addrs) max_concurrent_connections_try; *)
      (* Lwt.join (enum 0) |> ignore *)
    | PeerEvent (p, ev) ->
      handle_peer_event bt p ev
    end
    >>= loop
  in
  loop ()

let start bt =
  let port, _ = create_server (push_incoming_peer bt) in
  info "starting";
  Lwt_list.iter_p (fun tr ->
      Tracker.query tr bt.ih port bt.id >|= fun resp ->
      push_peers_received bt resp.Tracker.peers) (List.flatten bt.trackers) |> ignore;
  event_loop bt
    
(* let max_num_peers = 70 *)
(* let bittorrent_id_prefix = "-OC0001-" *)

(* (\* val add_peer : header:string -> ih:H.t -> id:H.t -> Tcp.socket -> unit Lwt.t *\) *)
(* let add_peer bt ~header ~ih ~id sock = *)
(*   if List.exists (fun p -> Peer.id p = id) bt.peers then *)
(*     Lwt.return () *)
(*   else begin *)
(*     let p = Peer.create ih id sock in *)
(*     if Peer.has_dht p then Dht.add_node bt.dht (Peer.addr p); *)
(*     Peer.writer_loop p push_msg p; *)
(*     Peer.reader_loop p push_msg p; *)
(*     if Peer.has_extensions p then Peer.send_extensions bt.port *)
(*     else Peer.send_bitfield p pieces *)
(*   end *)

(* let download cl = *)
(*   Lwt_list.iter_p *)
(*     (fun tr -> *)
(*       Tracker.query tr cl.ih 44443 cl.id >>= fun resp -> *)
(*       let peers = resp.Tracker.peers in *)
(*       let peers = List.map (fun (addr, port) -> Peer.of_sockaddr addr port) peers in *)
(*       Lwt_list.iter_p (fun p -> *)
(*           Peer.connect p >>= fun () -> *)
(*           Peer.handshake p ~id:cl.id ~ih:cl.ih >>= fun id' -> *)
(*           Peer.request_info p >>= fun s -> *)
(*           let info = Info.create (Get.run Bcode.bdecode s) in *)
(*           Info.pp Format.std_formatter info; *)
(*           Lwt.return ()) peers) cl.trackers *)

(* let of_magnet mg = *)
(*   let id = Word160.peer_id bittorrent_id_prefix in *)
(*   { id; ih = mg.Magnet.xt; trackers = List.map Tracker.create mg.Magnet.tr } *)
