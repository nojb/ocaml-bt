open Info

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

(* type sender_msg = *)
(*   | SendMsg of Wire.msg *)
(*   | SendPiece of int * int * int *)
(*   | SendCancel of int * int * int *)

(* let string_of_sender_msg = function *)
(*   | SendMsg msg -> *)
(*     Printf.sprintf "SendMsg: %s" (Wire.string_of_msg msg) *)
(*   | SendPiece (i, off, len) -> *)
(*     Printf.sprintf "SendPiece: index: %d offset: %d length: %d" i off len *)
(*   | SendCancel (i, off, len) -> *)
(*     Printf.sprintf "SendCancel: index: %d offset: %d length: %d" i off len *)

let failwith fmt =
  Printf.ksprintf failwith fmt

let max_pipelined_requests = 5
let lo_mark = 5
let hi_mark = 25
let default_block_size = 16 * 1024
let keepalive_delay = 20 (* FIXME *)
  
type piece_progress = {
  mutable index : int;
  mutable received : int;
  mutable last_offset : int;
  buffer : string
}

type event =
  | CHOKED of t
  | READY of t
  | PIECE_AVAILABLE of t * int
  | GOT_BITFIELD of t * Bits.t
  | PIECE_SENT of t * int
  | PIECE_COMPLETED of t * int * string
  | DISCONNECTED of t
  | ERROR of t * exn

and t = {
  sa : Lwt_unix.sockaddr;
  id : Word160.t;
  mutable we_interested : bool;
  mutable we_choking : bool;
  mutable peer_interested : bool;
  mutable peer_choking : bool;
  peer_pieces : Bits.t;
  mutable last_msg : int;
  mutable current : piece_progress option;
  mutable outstanding : (int * int * int) list;
  (* missing_pieces : int; *)
  (* last_msg_tick : int; *)
  (* last_piece_tick : int; *)
  (* mutable last_pn : int; *)
  pieces : Info.piece_info array;
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  write : [`Literal of string | `Block of int * int * int] -> unit;
  ul : float Lwt_react.S.t;
  dl : float Lwt_react.S.t;
  update_dl : int -> unit;
  update_ul : int -> unit;
  ticker : unit Lwt.t;
  handle_event : event -> unit;
  (* mutable handlers : (event -> unit) list; *)
  extensions : (string, int) Hashtbl.t;
  (* mutable ext_handshake : (string * Bcode.t) list *)
}

let string_of_sockaddr = function
  | Lwt_unix.ADDR_UNIX s ->
    s
  | Lwt_unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

let to_string pr =
  Printf.sprintf "%s@%s [%c%c|%c%c|%d/%d]"
    (Word160.to_hex_short pr.id)
    (string_of_sockaddr pr.sa)
    (if pr.we_choking then 'C' else 'c')
    (if pr.we_interested then 'I' else 'i')
    (if pr.peer_choking then 'C' else 'c')
    (if pr.peer_interested then 'I' else 'i')
    (Bits.count pr.peer_pieces) (Array.length pr.pieces)

let send_message peer message =
  let s = Wire.put message |> Put.run in
  `Literal s |> peer.write

let send_block peer i off len =
  `Block (i, off, len) |> peer.write
    
let handle_ut_metadata pr m =
  assert false
  (* let m = Bcode.from_string m in *)
  (* let msg_type = Bcode.find "msg_type" m |> Bcode.to_int in *)
  (* let piece = Bcode.find "piece" m |> Bcode.to_int in *)
  (* match msg_type with *)
  (* | 0 (\* request *\) -> *)
  (*   let id = Hashtbl.find "ut_metadata" pr.extensions in *)
  (*   if (\* has complete metadata *\) then *)
  (*     let d = *)
  (*       ["msg_type", Bcode.BInt 1L; *)
  (*        "piece", Bcode.BInt (Int64.of_int piece); *)
  (*        "total_size", (\* size of metadata *\)] *)
  (*     in *)
  (*     let msg = Bcode.bencode (Bcode.BDict d) @ (\* metadata piece *\) in *)
  (*     send pr (SendMsg Wire.Extended (id, msg)) *)
  (*   else *)
  (*     let d = *)
  (*       ["msg_type", Bcode.BInt 2L; *)
  (*        "piece", Bcode.BInt (Int64.of_int piece)] *)
  (*     in *)
  (*     send pr (SendMsg Wire.Extended (id, Bcode.bencode (Bcode.BDict d))) *)
  (* | 1 (\* data *\) -> *)
  (*   (\* add data to metainfo *\) *)
  (* | 2 (\* reject *\) -> *)
  (*   (\* fine, ignore *\) *)
  (* | _ -> *)
  (*   (\* An unrecognised msg_type must be ignored, see *)
  (*      http://www.bittorrent.org/beps/bep_0009.html *\) *)
  (*   Trace.infof "Unknown ut_metadata msg_type %d from %s, ignoring" *)
  (*     msg_type (to_string pr) *)

let supported_extensions = [
  1, ("ut_metadata", handle_ut_metadata)
]

let send_extended_hanshake peer =
  let m =
    List.map (fun (id, (name, _)) ->
        name, Bcode.BInt (Int64.of_int id)) supported_extensions
  in
  let m = Bcode.BDict ["m", Bcode.BDict m] in
  let m = Bcode.bencode m |> Put.run in
  send_message peer (Wire.EXTENDED (0, m))

let request_more_blocks peer =
  match peer.current with
  | None ->
    ()
  | Some c ->
    (* debug "requesting more blocks from %s for piece #%d" *)
      (* (to_string pr) c.index; *)
    let plen = peer.pieces.(c.index).Info.piece_length in
    while List.length peer.outstanding < max_pipelined_requests &&
      c.last_offset < plen do
      let blen = min default_block_size (plen-c.last_offset) in
      c.last_offset <- c.last_offset + blen;
      peer.outstanding <- (c.index, c.last_offset, blen) :: peer.outstanding;
      send_message peer (Wire.REQUEST (c.index, c.last_offset, blen))
    done
    
let download_piece pr index =
  match pr.current with
  | Some _ ->
    failwith "download_piece: peer already downloading piece; what is going on?"
  | None ->
    Trace.infof "Requesting #%d from %s" index (to_string pr);
    pr.current <- Some
        { index; received = 0; last_offset = 0;
          buffer = String.create (pr.pieces.(index).Info.piece_length) };
    request_more_blocks pr

let available_pieces pr =
  Bits.copy pr.peer_pieces

let requested_piece pr =
  match pr.current with
  | None -> None
  | Some c -> Some c.index

let got_choke peer =
  peer.peer_choking <- true;
  begin match peer.current with
    | None ->
      ()
    | Some c ->
      (* FIXME cancel pending requests *)
      peer.handle_event (CHOKED peer);
      (* List.iter (fun h -> h (CHOKED pr)) pr.handlers; *)
      (* Lwt_pipe.write self.piece_mgr_ch (PieceMgr.PutbackPiece c.index); *)
      peer.current <- None;
      ()
  end;
  Trace.infof "%s choked us" (to_string peer)

let got_unchoke peer =
  Trace.infof "%s unchoked us" (to_string peer);
  peer.peer_choking <- false;
  peer.handle_event (READY peer)
(* List.iter (fun h -> h (READY pr)) pr.handlers *)

let got_interested peer =
  if not peer.peer_interested then begin
    Trace.infof "%s is interested in us" (to_string peer);
    peer.peer_interested <- true
  end

let got_not_interested peer =
  if peer.peer_interested then begin
    Trace.infof "%s is no longer interested in us" (to_string peer);
    peer.peer_interested <- false;
  end
  (* Lwt_pipe.write self.choke_mgr_ch (ChokeMgr.PeerNotInterested self.id); *)
  (* Lwt.return_unit *)

let got_have peer i =
  if i < 0 || i >= Array.length peer.pieces then
    failwith "%s has invalid #%d" (to_string peer) i;
  Bits.set peer.peer_pieces i;
  Trace.infof "%s has obtained #%d and now has %d/%d pieces"
    (to_string peer) i (Bits.count peer.peer_pieces)
    (Array.length peer.pieces);
  peer.handle_event (PIECE_AVAILABLE (peer, i))
    (* List.iter (fun h -> h (PIECE_AVAILABLE (pr, index))) pr.handlers *)
  (* later : track interest, decr missing counter *)

let got_bitfield peer b =
  (* if Bits.count t.peer_pieces = 0 then begin *)
  (* FIXME padding, should only come after handshake *)
  Bits.blit b 0 peer.peer_pieces 0 (Bits.length peer.peer_pieces);
  peer.handle_event (GOT_BITFIELD (peer, peer.peer_pieces))
    (* List.iter (fun h -> h (GOT_BITFIELD (pr, pr.peer_pieces))) pr.handlers *)

let got_request peer i off len =
  if peer.we_choking then
    failwith "%s violating protocol, terminating exchange" (to_string peer);
  (* FIXME *)
  send_block peer i off len

let got_piece peer i off s =
  begin match peer.current with
    | None ->
      failwith "%s sent us a piece but we are not not downloading, terminating"
        (to_string peer)
    | Some c ->
      if c.index <> i then
        failwith "%s sent some blocks for unrequested piece, terminating" (to_string peer);
      (* FIXME check that the length is ok *)
      peer.outstanding <-
        List.filter (fun (i1, off1, _) -> not (i1 = i && off1 = off)) peer.outstanding;
      String.blit s 0 c.buffer off (String.length s);
      c.received <- c.received + String.length s;
      if c.received = peer.pieces.(i).Info.piece_length then begin
        peer.handle_event (PIECE_COMPLETED (peer, i, c.buffer));
        (* List.iter (fun h -> h (PIECE_COMPLETED (pr, index, c.buffer))) pr.handlers; *)
        peer.current <- None;
        peer.handle_event (READY peer)
        (* List.iter (fun h -> h (READY pr)) pr.handlers; *)
      end else
        request_more_blocks peer
  end

let got_cancel peer i off len =
  ()
  (* FIXME *)
  (* send peer (SendCancel (i, off, len)) *)

let got_extended_handshake peer m =
  let m = Get.run Bcode.bdecode m |> Bcode.find "m" |> Bcode.to_dict in
  List.iter (fun (name, id) ->
      let id = Bcode.to_int id in
      if id = 0 then Hashtbl.remove peer.extensions name
      else Hashtbl.replace peer.extensions name id) m;
  Trace.infof "%s supports EXTENDED: %s" (to_string peer)
    (String.concat " " (List.map (fun (name, _) -> name) m))

let got_extended peer id m =
  if List.mem_assoc id supported_extensions then
    let _, f = List.assoc id supported_extensions in
    f peer m
  else      
    Trace.infof "%s sent unsupported EXTENDED message %d" (to_string peer) id

let got_message peer message =
  Trace.recv (to_string peer) (Wire.string_of_message message);
  (* pr.update_dl (Wire.size_of_msg msg); *)
  match message with
  | Wire.KEEP_ALIVE -> ()
  | Wire.CHOKE -> got_choke peer
  | Wire.UNCHOKE -> got_unchoke peer
  | Wire.INTERESTED -> got_interested peer
  | Wire.NOT_INTERESTED -> got_not_interested peer
  | Wire.HAVE i -> got_have peer i
  | Wire.BITFIELD b -> got_bitfield peer b
  | Wire.REQUEST (i, off, len) -> got_request peer i off len
  | Wire.PIECE (i, off, s) -> got_piece peer i off s
  | Wire.CANCEL (i, off, len) -> got_cancel peer i off len
  | Wire.EXTENDED (0, m) -> got_extended_handshake peer m
  | Wire.EXTENDED (id, m) -> got_extended peer id m
  | _ ->
    Trace.infof "Unhandled message from %s: %s" (to_string peer) (Wire.string_of_message message)

let piece_completed peer i =
    send_message peer (Wire.HAVE i)

let is_interested peer =
  peer.peer_interested

let is_choked peer =
  peer.we_choking

let interesting peer =
  if not peer.we_interested then begin
    (* debug "we are interested in %s" (to_string pr); *)
    peer.we_interested <- true;
    send_message peer Wire.INTERESTED
  end

let not_interesting peer =
  if peer.we_interested then begin
    (* debug "we are no longer interested in %s" (to_string pr); *)
    peer.we_interested <- false;
    send_message peer Wire.NOT_INTERESTED
  end

let choke peer =
  if not peer.we_choking then begin
    (* debug "choking %s" (to_string pr); *)
    peer.we_choking <- true;
    send_message peer Wire.CHOKE
  end

let unchoke peer =
  if peer.we_choking then begin
    (* debug "unchoking %s" (to_string pr); *)
    peer.we_choking <- false;
    send_message peer Wire.UNCHOKE
  end

let make_rate r =
  let reset, updatereset = Lwt_react.E.create () in
  let total () = Lwt_react.S.fold (+) 0 r in
  let average () =
    let start = Unix.gettimeofday () in
    Lwt_react.S.map (fun x -> (float x) /. (Unix.gettimeofday () -. start)) (total ())
  in
  let rt = Lwt_react.S.switch (average ()) (Lwt_react.E.map average reset) in
  rt, updatereset

let rate_update_frequency = 3

let abort pr =
  Lwt.cancel pr.ticker;
  ignore (Lwt_io.abort pr.ic);
  ignore (Lwt_io.abort pr.oc);
  Trace.infof "Shutting down connection to %s" (to_string pr)

let writer_loop oc read_block write_queue =
  let keepalive_message = Wire.put Wire.KEEP_ALIVE |> Put.run in
  let rec loop () =
    Lwt.pick [Lwt_stream.next write_queue >|= (fun x -> `Write x);
              Lwt_unix.sleep (float keepalive_delay) >|= fun () -> `Timeout] >>= function
    | `Write (`Literal s) ->
      Lwt_io.write oc s >>= loop
    | `Write (`Block (i, off, len)) ->
      read_block i off len >>= fun s ->
      Wire.PIECE (i, off, s) |> Wire.put |> Put.run |> Lwt_io.write oc >>= loop
    | `Timeout ->
      Lwt_io.write oc keepalive_message >>= loop
  in
  Lwt.catch loop
    (fun exn ->
       Trace.infof ~exn "Peer write error";
       Lwt.fail exn)

let create sa id ic oc (minfo : Info.t) read_block handle_event =
  let write_queue, write = Lwt_stream.create () in
  let dl, update_dl = Lwt_react.E.create () in
  let ul, update_ul = Lwt_react.E.create () in
  let dl, resetdl = make_rate dl in
  let ul, resetul = make_rate ul in
  let rec ticker () =
    Lwt_unix.sleep (float rate_update_frequency) >>= fun () ->
    resetdl ();
    resetul ();
    ticker ()
  in
  let peer =
    { sa;
      id;
      we_interested = false;
      we_choking = true;
      peer_interested = false;
      peer_choking = true;
      peer_pieces = Bits.create (Array.length minfo.pieces);
      outstanding = [];
      last_msg = 0;
      current = None;
      pieces = minfo.pieces;
      ic;
      oc;
      write = (fun s -> write (Some s));
      ul;
      dl;
      update_dl;
      update_ul;
      ticker = ticker ();
      handle_event;
      (* handlers = []; *)
      extensions = Hashtbl.create 17 }
  in
  let reader ic =
    Lwt.catch
      (fun () ->
         Lwt_stream.iter (got_message peer)
           (Lwt_stream.from (fun () -> Wire.read ic >|= fun msg -> Some msg)))
      (fun exn ->
         Trace.infof ~exn "Peer read error";
         Lwt.fail exn)
  in
  let rd = reader ic in
  Lwt.on_failure rd (fun _ -> abort peer);
  let wr = writer_loop oc read_block write_queue in
  Lwt.on_failure wr (fun _ -> abort peer);
  (* if Bits.count stats.completed > 0 then send pr (SendMsg (Wire.BitField stats.completed)); *)
  peer
  (* Lwt.catch *)
  (*   (fun () -> *)
  (*   Lwt_pipe.iter_s (handle_message self) ch *)
  (*   finally *)
  (*   begin *)
  (*     Lwt.cancel self.ticker; *)
  (*     ignore (Lwt_io.close self.ic); *)
  (*     ignore (Lwt_io.close self.oc); *)
  (*     Lwt_pipe.write self.peer_mgr_ch (Disconnect id); *)
  (*     Lwt_pipe.write self.piece_mgr_ch (PieceMgr.PeerUnHave (Bits.to_list self.peer_pieces)); *)
  (*     Lwt_pipe.write self.choke_mgr_ch (ChokeMgr.RemovePeer id); *)
  (*     match self.current with *)
  (*     | Some c -> *)
  (*       Lwt_pipe.write self.piece_mgr_ch (PieceMgr.PutbackPiece c.index); *)
  (*       Lwt.return_unit *)
  (*     | None -> *)
  (*       Lwt.return_unit *)
  (*   end *)

let ul pr =
  Lwt_react.S.value pr.ul

let dl pr =
  Lwt_react.S.value pr.dl

(* let add_handler pr h = *)
(*   pr.handlers <- h :: pr.handlers *)
