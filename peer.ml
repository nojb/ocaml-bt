open Info

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type sender_msg =
  | SendMsg of Wire.msg
  | SendPiece of int * Wire.block
  | SendCancel of int * Wire.block

let string_of_sender_msg = function
  | SendMsg msg ->
    Printf.sprintf "SendMsg: %s" (Wire.string_of_msg msg)
  | SendPiece (index, Wire.Block (offset, length)) ->
    Printf.sprintf "SendPiece: index: %d offset: %d length: %d" index offset length
  | SendCancel (index, Wire.Block (offset, length)) ->
    Printf.sprintf "SendCancel: index: %d offset: %d length: %d"
      index offset length

let failwith fmt =
  Printf.ksprintf failwith fmt

let max_pipelined_requests = 5
let lo_mark = 5
let hi_mark = 25
let default_block_size = 16 * 1024

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
  mutable outstanding : (int * Wire.block) list;
  (* missing_pieces : int; *)
  (* last_msg_tick : int; *)
  (* last_piece_tick : int; *)
  (* mutable last_pn : int; *)
  pieces : Info.piece_info array;
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  write : sender_msg option -> unit;
  ul : float Lwt_react.S.t;
  dl : float Lwt_react.S.t;
  update_dl : int -> unit;
  update_ul : int -> unit;
  ticker : unit Lwt.t;
  mutable handlers : (event -> unit) list;
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

let send pr msg =
  pr.last_msg <- 0;
  pr.write (Some msg)
    
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

let send_extended_hanshake pr =
  let m =
    List.map (fun (id, (name, _)) ->
        name, Bcode.BInt (Int64.of_int id)) supported_extensions
  in
  let m = Bcode.BDict ["m", Bcode.BDict m] in
  let m = Bcode.bencode m |> Put.run in
  send pr (SendMsg (Wire.Extended (0, m)))

let request_more_blocks pr =
  match pr.current with
  | None ->
    ()
  | Some c ->
    (* debug "requesting more blocks from %s for piece #%d" *)
      (* (to_string pr) c.index; *)
    let plen = pr.pieces.(c.index).Info.piece_length in
    while List.length pr.outstanding < max_pipelined_requests &&
      c.last_offset < plen do
      let blen = min default_block_size (plen-c.last_offset) in
      let b = Wire.Block (c.last_offset, blen) in
      c.last_offset <- c.last_offset + blen;
      pr.outstanding <- (c.index, b) :: pr.outstanding;
      send pr (SendMsg (Wire.Request (c.index, b)))
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

let handle_peer_msg pr msg =
  Trace.recv (to_string pr) (Wire.string_of_msg msg);
  (* pr.update_dl (Wire.size_of_msg msg); *)
  match msg with
  | Wire.KeepAlive ->
    ()
  | Wire.Choke ->
    pr.peer_choking <- true;
    begin match pr.current with
      | None ->
        ()
      | Some c ->
        (* FIXME cancel pending requests *)
        List.iter (fun h -> h (CHOKED pr)) pr.handlers;
        (* Lwt_pipe.write self.piece_mgr_ch (PieceMgr.PutbackPiece c.index); *)
        pr.current <- None;
        ()
    end;
    Trace.infof "%s choked us" (to_string pr)
  | Wire.Unchoke ->
    Trace.infof "%s unchoked us" (to_string pr);
    pr.peer_choking <- false;
    List.iter (fun h -> h (READY pr)) pr.handlers
  | Wire.Interested ->
    if not pr.peer_interested then begin
      Trace.infof "%s is interested in us" (to_string pr);
      pr.peer_interested <- true
    end
  | Wire.NotInterested ->
    if pr.peer_interested then begin
      Trace.infof "%s is no longer interested in us" (to_string pr);
      pr.peer_interested <- false;
    end
  (* Lwt_pipe.write self.choke_mgr_ch (ChokeMgr.PeerNotInterested self.id); *)
  (* Lwt.return_unit *)
  | Wire.Have index ->
    if index < 0 || index >= Array.length pr.pieces then
      failwith "%s has invalid #%d" (to_string pr) index;
    Bits.set pr.peer_pieces index;
    Trace.infof "%s has obtained #%d and now has %d/%d pieces"
      (to_string pr) index (Bits.count pr.peer_pieces)
      (Array.length pr.pieces);
    List.iter (fun h -> h (PIECE_AVAILABLE (pr, index))) pr.handlers
  (* later : track interest, decr missing counter *)
  | Wire.BitField bits ->
    (* if Bits.count t.peer_pieces = 0 then begin *)
    (* FIXME padding, should only come after handshake *)
    Bits.blit bits 0 pr.peer_pieces 0 (Bits.length pr.peer_pieces);
    List.iter (fun h -> h (GOT_BITFIELD (pr, pr.peer_pieces))) pr.handlers
  | Wire.Request (index, block) ->
    if pr.we_choking then
      failwith "%s violating protocol, terminating exchange" (to_string pr);
    send pr (SendPiece (index, block))
  | Wire.Piece (index, offset, block) ->
    begin match pr.current with
      | None ->
        failwith "%s sent us a piece but we are not not downloading, terminating"
          (to_string pr)
      | Some c ->
        if c.index <> index then
          failwith "%s sent some blocks for unrequested piece, terminating" (to_string pr);
        (* FIXME check that the length is ok *)
        pr.outstanding <-
          List.filter (fun (i, Wire.Block (o, _)) ->
              not (i = index && o = offset)) pr.outstanding;
        String.blit block 0 c.buffer offset (String.length block);
        c.received <- c.received + String.length block;
        if c.received = pr.pieces.(index).Info.piece_length then begin
          List.iter (fun h -> h (PIECE_COMPLETED (pr, index, c.buffer))) pr.handlers;
          pr.current <- None;
          List.iter (fun h -> h (READY pr)) pr.handlers;
        end else
          request_more_blocks pr
    end
  | Wire.Cancel (index, block) ->
    send pr (SendCancel (index, block))
  | Wire.Extended (0, m) ->
    let m = Get.run Bcode.bdecode m |> Bcode.find "m" |> Bcode.to_dict in
    List.iter (fun (name, id) ->
        let id = Bcode.to_int id in
        if id = 0 then Hashtbl.remove pr.extensions name
        else Hashtbl.replace pr.extensions name id) m;
    Trace.infof "%s supports EXTENDED: %s" (to_string pr)
      (String.concat " " (List.map (fun (name, _) -> name) m))
  | Wire.Extended (id, m) ->
    if List.mem_assoc id supported_extensions then
      let _, f = List.assoc id supported_extensions in
      f pr m
    else      
      Trace.infof "%s sent unsupported EXTENDED message %d" (to_string pr) id
  | msg ->
    Trace.infof "Unhandled message from %s: %s" (to_string pr) (Wire.string_of_msg msg)

let handle_timer_tick pr =
  let keep_alive () =
    if pr.last_msg >= 24 then
      send pr (SendMsg Wire.KeepAlive)
    else
      pr.last_msg <- pr.last_msg + 1
  in
  keep_alive ()
  (* later- tell Status, tell ChokeMgr *)

let piece_completed pr index =
    send pr (SendMsg (Wire.Have index))

let is_interested pr =
  pr.peer_interested

let is_choked pr =
  pr.we_choking

let interesting pr =
  if not pr.we_interested then begin
    (* debug "we are interested in %s" (to_string pr); *)
    pr.we_interested <- true;
    send pr (SendMsg Wire.Interested)
  end

let not_interesting pr =
  if pr.we_interested then begin
    (* debug "we are no longer interested in %s" (to_string pr); *)
    pr.we_interested <- false;
    send pr (SendMsg Wire.NotInterested)
  end

let choke pr =
  if not pr.we_choking then begin
    (* debug "choking %s" (to_string pr); *)
    pr.we_choking <- true;
    send pr (SendMsg Wire.Choke)
  end

let unchoke pr =
  if pr.we_choking then begin
    (* debug "unchoking %s" (to_string pr); *)
    pr.we_choking <- false;
    send pr (SendMsg Wire.Unchoke)
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

let create stats sa id ic oc (minfo : Info.t) store =
  let write_stream, write = Lwt_stream.create () in
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
  let pr =
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
      write;
      ul;
      dl;
      update_dl;
      update_ul;
      ticker = ticker ();
      handlers = [];
      extensions = Hashtbl.create 17 }
  in
  let reader ic =
    Lwt.catch
      (fun () ->
         Lwt_stream.iter (handle_peer_msg pr)
           (Lwt_stream.from (fun () -> Wire.read ic >|= fun msg -> Some msg)))
      (fun exn ->
         Trace.infof ~exn "Peer read error";
         Lwt.fail exn)
  in
  let writer oc =
    Lwt.catch
      (fun () ->
         Lwt_stream.iter_s (function
             | SendMsg msg ->
               Wire.write oc msg >>= fun sz ->
               (* should notify somene  *)
               Trace.sent (to_string pr) (Wire.string_of_msg msg);
               Lwt.return_unit
             | SendPiece (i, b) ->
               let Wire.Block (off, _) = b in
               Store.read_block store i b >>= fun s ->
               Wire.write oc (Wire.Piece (i, off, s)) >>= fun sz ->
               Trace.sent (to_string pr) (Wire.string_of_msg (Wire.Piece (i, off, s)));
               (* pr.stats.uploaded <- *)
               (*   Int64.add pr.stats.uploaded (Int64.of_int (String.length s)); *)
               (* should notify someone *)
               Lwt.return_unit
             | _ as msg ->
               Trace.infof "Unhandled peer write command: %s" (string_of_sender_msg msg);
               Lwt.return_unit) write_stream)
      (fun exn ->
         Trace.infof ~exn "Peer write error";
         Lwt.fail exn)
  in
  let rd = reader ic in
  Lwt.on_failure rd (fun _ -> abort pr);
  let wr = writer oc in
  Lwt.on_failure wr (fun _ -> abort pr);
  if Bits.count stats.completed > 0 then send pr (SendMsg (Wire.BitField stats.completed));
  pr
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

let add_handler pr h =
  pr.handlers <- h :: pr.handlers
