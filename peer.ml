open Msg

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug = Proc.debug

let failwith_lwt fmt =
  Printf.ksprintf (fun msg -> raise_lwt (Failure msg)) fmt

let max_pipelined_requests = 5

let string_of_msg = function
  | PeerMsg msg ->
    Printf.sprintf "PeerMsg: %s" (Wire.string_of_msg msg)
  | BytesSent sz ->
    Printf.sprintf "BytesSent: %d" sz
  | Tick ->
    "Tick"
  | ReceiverAborted exn ->
    Printf.sprintf "ReceiverAborted: %s" (Printexc.to_string exn)
  | SenderAborted exn ->
    Printf.sprintf "SenderAborted: %s" (Printexc.to_string exn)
  | PieceCompleted index ->
    Printf.sprintf "PieceCompleted: index: %d" index
  | Choke ->
    "Choke"
  | UnChoke ->
    "UnChoke"

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
  mutable outstanding : (int * Wire.block) list;
  (* missing_pieces : int; *)
  (* up_rate : rate; *)
  (* down_rate : rate; *)
  (* last_msg_tick : int; *)
  (* last_piece_tick : int; *)
  (* interesting_pieces : PieceSet.t; *)
  (* mutable last_pn : int; *)
  pieces : Info.piece_info array;
  peer_mgr_ch : Msg.peer_mgr_msg Lwt_pipe.t;
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  sender_ch : Msg.sender_msg Lwt_pipe.t;
  piece_mgr_ch : PieceMgr.msg Lwt_pipe.t;
  choke_mgr_ch : ChokeMgr.msg Lwt_pipe.t;
  ul : float Lwt_react.S.t;
  dl : float Lwt_react.S.t;
  update_dl : int -> unit;
  update_ul : int -> unit;
  ticker : unit Lwt.t;
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
      let b = Wire.Block (c.last_offset, blen) in
      c.last_offset <- c.last_offset + blen;
      t.outstanding <- (c.index, b) :: t.outstanding;
      send t (Msg.SendMsg (Wire.Request (c.index, b)))
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

let handle_peer_msg t msg =
  t.update_dl (Wire.size_of_msg msg);
  match msg with
  | Wire.KeepAlive ->
    Lwt.return_unit
  | Wire.Choke ->
    t.peer_choking <- true;
    (* Lwt_pipe.write t.choke_mgr_ch (ChokeMgr.PeerChoked t.id); *)
    begin match t.current with
    | None ->
      Lwt.return_unit
    | Some c ->
      (* FIXME cancel pending requests *)
      Lwt_pipe.write t.piece_mgr_ch (`PutbackPiece c.index);
      t.current <- None;
      Lwt.return_unit
    end
  | Wire.Unchoke ->
    t.peer_choking <- false;
    (* Lwt_pipe.write t.choke_mgr_ch (ChokeMgr.PeerUnChoked t.id); *)
    request_new_piece t
  | Wire.Interested ->
    t.peer_interested <- true;
    Lwt_pipe.write t.choke_mgr_ch (ChokeMgr.PeerInterested t.id);
    Lwt.return_unit
  | Wire.NotInterested ->
    t.peer_interested <- false;
    Lwt_pipe.write t.choke_mgr_ch (ChokeMgr.PeerNotInterested t.id);
    Lwt.return_unit
  | Wire.Have index ->
    if index >= 0 && index < Array.length t.pieces then begin
      Bits.set t.peer_pieces index;
      tell_peer_has t [index] >>= fun interested ->
      if interested then begin
        t.we_interested <- true;
        send t (SendMsg Wire.Interested)
      end;
      Lwt.return_unit
      (* later : track interest, decr missing counter *)
    end else
      raise_lwt (UnknownPiece index)
  | Wire.BitField bits ->
    (* if Bits.count t.peer_pieces = 0 then begin *)
    (* FIXME padding, should only come after handshake *)
    Bits.blit bits 0 t.peer_pieces 0 (Bits.length t.peer_pieces);
    tell_peer_has t (Bits.to_list bits) >>= fun interested ->
    t.we_interested <- interested;
    send t (SendMsg (if interested then Wire.Interested else Wire.NotInterested));
    Lwt.return_unit
  | Wire.Request (index, block) ->
    if t.we_choking then
      failwith_lwt "peer violating protocol, terminating exchange"
    else begin
      send t (SendPiece (index, block));
      Lwt.return_unit
    end
  | Wire.Piece (index, offset, block) ->
    begin match t.current with
    | None ->
      failwith_lwt "Peer received piece while not downloading, terminating"
    | Some c ->
      if c.index <> index then
        failwith_lwt "Peer sent some blocks for unrequested piece, terminating"
      else begin
        (* FIXME check that the length is ok *)
        t.outstanding <-
          List.filter (fun (i, Wire.Block (o, _)) ->
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
  | Wire.Cancel (index, block) ->
    send t (SendCancel (index, block));
    Lwt.return_unit
  | msg ->
    debug t.id "Unahandled: %s" (Wire.string_of_msg msg)

let handle_timer_tick t =
  let keep_alive () =
    if t.last_msg >= 24 then
      send t (SendMsg Wire.KeepAlive)
    else
      t.last_msg <- t.last_msg + 1
  in
  keep_alive ();
  Lwt_pipe.write t.choke_mgr_ch
    (ChokeMgr.PeerRateUpdate (t.id, Lwt_react.S.value t.ul, Lwt_react.S.value t.dl))
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
    send t (SendMsg (Wire.Have index));
    Lwt.return_unit
  | ReceiverAborted _
  | SenderAborted _ ->
    raise_lwt Exit
  | BytesSent sz ->
    t.update_ul sz;
    Lwt.return_unit
  | Choke ->
    t.we_choking <- true;
    Lwt_pipe.write t.choke_mgr_ch (ChokeMgr.PeerChoked t.id);
    Lwt.return_unit
  | UnChoke ->
    t.we_choking <- false;
    Lwt_pipe.write t.choke_mgr_ch (ChokeMgr.PeerUnChoked t.id);
    Lwt.return_unit
  (* | msg -> *)
  (*   debug t.id "Unhandled: %s" (string_of_msg msg) *)

let receiver ic ch _ =
  try_lwt
    Lwt_stream.iter (fun msg -> Lwt_pipe.write ch (Msg.PeerMsg msg))
      (Lwt_stream.from (fun () -> Wire.read ic >|= fun msg -> Some msg))
  with
  | exn ->
    Lwt_pipe.write ch (Msg.ReceiverAborted exn);
    Lwt.return_unit
  finally
    Lwt_io.close ic

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

let start ic oc ~peer_mgr_ch ih ~pieces ~piece_mgr_ch ~fs_ch ~choke_mgr_ch =
  let sender_ch = Lwt_pipe.create () in
  let ch = Lwt_pipe.create () in
  let dl, update_dl = Lwt_react.E.create () in
  let ul, update_ul = Lwt_react.E.create () in
  let dl, resetdl = make_rate dl in
  let ul, resetul = make_rate ul in
  let rec ticker id =
    Lwt_unix.sleep (float rate_update_frequency) >>= fun () ->
    Lwt_pipe.write choke_mgr_ch
      (ChokeMgr.PeerRateUpdate (id, Lwt_react.S.value ul, Lwt_react.S.value dl));
    resetdl ();
    resetul ();
    ticker id
  in
  let run id =
    let t =
      { we_interested = true; we_choking = false;
        peer_interested = true; peer_choking = false;
        peer_pieces = Bits.create (Array.length pieces);
        outstanding = [];
        last_msg = 0;
        current = None;
        pieces;
        peer_mgr_ch;
        ic; oc;
        sender_ch;
        piece_mgr_ch;
        choke_mgr_ch;
        ul; dl;
        update_dl; update_ul;
        ticker = ticker id;
        id }
    in
    ignore (Sender.start oc ~ch:sender_ch ~fs_ch ~peer_ch:ch);
    Proc.async (receiver ic ch);
    Lwt_pipe.write peer_mgr_ch (Connect (id, ch));
    Lwt_pipe.write choke_mgr_ch (ChokeMgr.AddPeer (id, ch));
    let mv = Lwt_mvar.create_empty () in
    Lwt_pipe.write piece_mgr_ch (`GetDone mv);
    Lwt_mvar.take mv >>= fun bits ->
    if Bits.count bits > 0 then send t (SendMsg (Wire.BitField bits));
    try_lwt
      Lwt_pipe.iter_s (handle_message t) ch
    finally
      begin
        Lwt.cancel t.ticker;
        ignore (Lwt_io.close t.ic);
        ignore (Lwt_io.close t.oc);
        Lwt_pipe.write t.peer_mgr_ch (Disconnect id);
        Lwt_pipe.write t.piece_mgr_ch (`PeerUnHave (Bits.to_list t.peer_pieces));
        Lwt_pipe.write t.choke_mgr_ch (ChokeMgr.RemovePeer id);
        match t.current with
        | Some c ->
          Lwt_pipe.write t.piece_mgr_ch (`PutbackPiece c.index);
          Lwt.return_unit
        | None ->
          Lwt.return_unit
      end
  in
  Proc.spawn ~name:"Peer" run (fun _ -> Lwt.return_unit)
