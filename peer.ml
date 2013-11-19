open Msg

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug = Proc.debug

let failwith_lwt fmt =
  Printf.ksprintf (fun msg -> raise_lwt (Failure msg)) fmt

let max_pipelined_requests = 5

let string_of_msg = function
  | PeerMsg msg ->
    Printf.sprintf "%s" (string_of_peer_msg msg)
  | BytesSent sz ->
    Printf.sprintf "BytesSent: %d" sz
  | Tick ->
    "Tick"
  | PieceCompleted index ->
    Printf.sprintf "PieceCompleted: index: %d" index

type piece_progress = {
  mutable index : int;
  mutable received : int;
  mutable last_offset : int;
  buffer : string
}

type t = {
  mutable we_interested : bool;
  mutable we_choking : bool;
  mutable peer_interested : bool;
  mutable peer_choking : bool;
  peer_pieces : Bits.t;
  mutable last_msg : int;
  mutable current : piece_progress option;
  mutable outstanding : (int * Msg.block) list;
  (* missing_pieces : int; *)
  (* up_rate : rate; *)
  (* down_rate : rate; *)
  (* last_msg_tick : int; *)
  (* last_piece_tick : int; *)
  (* interesting_pieces : PieceSet.t; *)
  (* mutable last_pn : int; *)
  pieces : Info.piece_info array;
  peer_mgr_ch : Msg.peer_mgr_msg Lwt_pipe.t;
  sender_ch : Msg.sender_msg Lwt_pipe.t;
  piece_mgr_ch : PieceMgr.msg Lwt_pipe.t;
  id : Proc.Id.t
}

let lo_mark = 5
let hi_mark = 25
let default_block_size = 16 * 1024

let send t msg =
  t.last_msg <- 0;
  Lwt_pipe.write t.sender_ch msg

let request_more_blocks t =
  match t.current with
  | None ->
    Lwt.return_unit
  | Some c ->
    debug t.id "requesting more blocks from the PieceMgr for piece%4d"
      c.index >>= fun () ->
    let plen = t.pieces.(c.index).Info.piece_length in
    while List.length t.outstanding < max_pipelined_requests &&
      c.last_offset < plen do
      let blen = min default_block_size (plen-c.last_offset) in
      let b = Msg.Block (c.last_offset, blen) in
      c.last_offset <- c.last_offset + blen;
      t.outstanding <- (c.index, b) :: t.outstanding;
      send t (Msg.SendMsg (Request (c.index, b)))
    done;
    Lwt.return_unit

let request_new_piece t =
  let mv = Lwt_mvar.create_empty () in
  Lwt_pipe.write t.piece_mgr_ch (`GrabPiece (t.peer_pieces, mv));
  Lwt_mvar.take mv >>= function
    | None ->
      t.current <- None;
      Lwt.return_unit
    | Some index ->
      t.current <- Some { index; received = 0; last_offset = 0; buffer =
        String.create (t.pieces.(index).Info.piece_length) };
      request_more_blocks t

let tell_peer_has t pns : bool Lwt.t =
  let mv = Lwt_mvar.create_empty () in
  Lwt_pipe.write t.piece_mgr_ch (`PeerHave (pns, mv));
  Lwt_mvar.take mv

exception UnknownPiece of int

let handle_peer_msg t = function
  | KeepAlive ->
    Lwt.return_unit
  | Choke ->
    t.peer_choking <- true;
    begin match t.current with
    | None ->
      Lwt.return_unit
    | Some c ->
      (* FIXME cancel pending requests *)
      Lwt_pipe.write t.piece_mgr_ch (`PutbackPiece c.index);
      t.current <- None;
      Lwt.return_unit
    end
  | Unchoke ->
    t.peer_choking <- false;
    request_new_piece t
  | Interested ->
    t.peer_interested <- true;
    Lwt.return_unit
  | NotInterested ->
    t.peer_interested <- false;
    Lwt.return_unit
  | Have index ->
    if index >= 0 && index < Array.length t.pieces then begin
      Bits.set t.peer_pieces index;
      tell_peer_has t [index] >>= fun interested ->
      if interested then begin
        t.we_interested <- true;
        send t (SendMsg Interested)
      end;
      Lwt.return_unit
      (* later : track interest, decr missing counter *)
    end else
      raise_lwt (UnknownPiece index)
  | BitField bits ->
    (* if Bits.count t.peer_pieces = 0 then begin *)
    (* FIXME padding, should only come after handshake *)
    Bits.blit bits 0 t.peer_pieces 0 (Bits.length t.peer_pieces);
    tell_peer_has t (Bits.to_list bits) >>= fun interested ->
    t.we_interested <- interested;
    send t (SendMsg (if interested then Interested else NotInterested));
    Lwt.return_unit
  | Request (index, block) ->
    if t.we_choking then
      failwith_lwt "peer violating protocol, terminating exchange"
    else begin
      send t (SendPiece (index, block));
      Lwt.return_unit
    end
  | Piece (index, offset, block) ->
    begin match t.current with
    | None ->
      failwith_lwt "Peer received piece while not downloading, terminating"
    | Some c ->
      if c.index <> index then
        failwith_lwt "Peer sent some blocks for unrequested piece, terminating"
      else begin
        (* FIXME check that the length is ok *)
        t.outstanding <-
          List.filter (fun (i, Msg.Block (o, _)) ->
            not (i = index && o = offset)) t.outstanding;
        String.blit block 0 c.buffer offset (String.length block);
        c.received <- c.received + String.length block;
        if c.received = t.pieces.(index).Info.piece_length then begin
          Lwt_pipe.write t.piece_mgr_ch (`PieceReceived (index, c.buffer));
          request_new_piece t
        end else
          request_more_blocks t
      end
    end
  | Cancel (index, block) ->
    send t (SendCancel (index, block));
    Lwt.return_unit
  | msg ->
    debug t.id "Unahandled: %s" (string_of_peer_msg msg)

let handle_timer_tick t =
  let keep_alive () =
    if t.last_msg >= 24 then
      send t (SendMsg KeepAlive)
    else
      t.last_msg <- t.last_msg + 1
  in
  keep_alive ()
  (* later- tell Status, tell ChokeMgr *)

let handle_message t msg : unit Lwt.t =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | PeerMsg msg ->
    handle_peer_msg t msg
  | Tick ->
    handle_timer_tick t;
    Lwt.return_unit
  | PieceCompleted index ->
    send t (SendMsg (Have index));
    Lwt.return_unit
  | msg ->
    debug t.id "Unhandled: %s" (string_of_msg msg)

let start_peer ~super_ch ~peer_mgr_ch ~ch ih ~pieces
  ~sender_ch ~piece_mgr_ch =
  let run id =
    let t =
      { we_interested = true;
        we_choking = false;
        peer_interested = true;
        peer_choking = false;
        peer_pieces = Bits.create (Array.length pieces);
        outstanding = [];
        last_msg = 0;
        current = None;
        pieces;
        peer_mgr_ch;
        sender_ch;
        piece_mgr_ch;
        id }
    in
    Lwt_pipe.write peer_mgr_ch (Connect (id, ch));
    (* let mv = Lwt_mvar.create_empty () in *)
    (* Lwt_pipe.write piece_mgr_ch (`GetDone mv); *)
    (* Lwt_mvar.take mv >>= fun bits -> *)
    (* send t (SendMsg (BitField bits)); *)
    try_lwt
      Lwt_pipe.iter_s (handle_message t) ch
    with
    | exn ->
      Lwt_pipe.write t.peer_mgr_ch (Disconnect id);
      begin match t.current with
      | Some c -> Lwt_pipe.write t.piece_mgr_ch (`PutbackPiece c.index)
      | None -> ()
      end;
      raise exn
  in
  Proc.spawn ~name:"Peer" run (Super.default_stop super_ch)

let start ic oc ~peer_mgr_ch ih ~pieces ~piece_mgr_ch =
  let sender_ch = Lwt_pipe.create () in
  let ch = Lwt_pipe.create () in
  [
    Worker (Sender.start oc ~ch:sender_ch ~peer_ch:ch);
    Worker (Receiver.start ic ~peer_ch:ch);
    Worker (start_peer ~peer_mgr_ch ~ch ih ~pieces ~sender_ch ~piece_mgr_ch)
  ]
