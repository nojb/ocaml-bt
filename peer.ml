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
  addr : Unix.inet_addr;
  port : int;
  peer_id : Info.PeerId.t;
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

let to_string self =
  Printf.sprintf "peer://%s:%d/%s [%c%c|%c%c|%d]"
    (Unix.string_of_inet_addr self.addr)
    self.port
    (Info.PeerId.to_hex_short self.peer_id)
    (if self.we_choking then 'C' else 'c')
    (if self.we_interested then 'I' else 'i')
    (if self.peer_choking then 'C' else 'c')
    (if self.peer_interested then 'I' else 'i')
    (Bits.count self.peer_pieces)

let lo_mark = 5
let hi_mark = 25
let default_block_size = 16 * 1024

let send self msg =
  self.last_msg <- 0;
  Lwt_pipe.write self.sender_ch msg

let request_more_blocks self =
  match self.current with
  | None ->
    Lwt.return_unit
  | Some c ->
    debug self.id "requesting more blocks from the PieceMgr for piece%4d"
      c.index >>= fun () ->
    let plen = self.pieces.(c.index).Info.piece_length in
    while List.length self.outstanding < max_pipelined_requests &&
      c.last_offset < plen do
      let blen = min default_block_size (plen-c.last_offset) in
      let b = Wire.Block (c.last_offset, blen) in
      c.last_offset <- c.last_offset + blen;
      self.outstanding <- (c.index, b) :: self.outstanding;
      send self (Msg.SendMsg (Wire.Request (c.index, b)))
    done;
    Lwt.return_unit

let request_new_piece self =
  let mv = Lwt_mvar.create_empty () in
  Lwt_pipe.write self.piece_mgr_ch (PieceMgr.GrabPiece (self.peer_pieces, mv));
  Lwt_mvar.take mv >>= function
    | None ->
      self.current <- None;
      Lwt.return_unit
    | Some index ->
      self.current <- Some
        { index; received = 0; last_offset = 0;
          buffer = String.create (self.pieces.(index).Info.piece_length) };
      request_more_blocks self

let tell_peer_has self pns : bool Lwt.t =
  let mv = Lwt_mvar.create_empty () in
  Lwt_pipe.write self.piece_mgr_ch (PieceMgr.PeerHave (pns, mv));
  Lwt_mvar.take mv

exception UnknownPiece of int

let peer_choke self =
  self.peer_choking <- true;
  match self.current with
  | None ->
    Lwt.return_unit
  | Some c ->
    (* FIXME cancel pending requests *)
    Lwt_pipe.write self.piece_mgr_ch (PieceMgr.PutbackPiece c.index);
    self.current <- None;
    Lwt.return_unit

let peer_unchoke self =
  debug self.id "Peer %s now accepting requests" (to_string self) >>= fun () ->
  self.peer_choking <- false;
  request_new_piece self

let peer_interested self =
  self.peer_interested <- true;
  Lwt_pipe.write self.choke_mgr_ch (ChokeMgr.PeerInterested self.id);
  Lwt.return_unit

let peer_not_interested self =
  self.peer_interested <- false;
  Lwt_pipe.write self.choke_mgr_ch (ChokeMgr.PeerNotInterested self.id);
  Lwt.return_unit

let peer_have self index =
  if index >= 0 && index < Array.length self.pieces then begin
    Bits.set self.peer_pieces index;
    debug self.id "Peer %s now has %d [%d/%d]"
      (to_string self) index (Bits.count self.peer_pieces)
      (Array.length self.pieces) >>= fun () ->
    tell_peer_has self [index] >>= fun interested ->
    if interested && not self.we_interested then begin
      ignore (debug self.id "Telling %s we are interested" (to_string self));
      self.we_interested <- true;
      send self (SendMsg Wire.Interested)
    end;
    Lwt.return_unit
    (* later : track interest, decr missing counter *)
  end else
    raise_lwt (UnknownPiece index)

let peer_bitfield self bits =
  (* if Bits.count t.peer_pieces = 0 then begin *)
  (* FIXME padding, should only come after handshake *)
  debug self.id "Recorded bitfield from %s with %d piece(s) [%d/%d]"
    (to_string self) (Bits.count bits) (Bits.count self.peer_pieces)
    (Array.length self.pieces) >>= fun () ->
  Bits.blit bits 0 self.peer_pieces 0 (Bits.length self.peer_pieces);
  tell_peer_has self (Bits.to_list bits) >>= fun interested ->
  self.we_interested <- interested;
  send self (SendMsg (if interested then Wire.Interested else Wire.NotInterested));
  Lwt.return_unit

let peer_request self index block =
  if self.we_choking then
    failwith_lwt "peer violating protocol, terminating exchange"
  else begin
    send self (SendPiece (index, block));
    Lwt.return_unit
  end

let peer_piece self index offset block =
  match self.current with
  | None ->
    failwith_lwt "Peer received piece while not downloading, terminating"
  | Some c ->
    if c.index <> index then
      failwith_lwt "Peer sent some blocks for unrequested piece, terminating"
    else begin
      (* FIXME check that the length is ok *)
      self.outstanding <-
        List.filter (fun (i, Wire.Block (o, _)) ->
          not (i = index && o = offset)) self.outstanding;
      String.blit block 0 c.buffer offset (String.length block);
      c.received <- c.received + String.length block;
      if c.received = self.pieces.(index).Info.piece_length then begin
        Lwt_pipe.write self.piece_mgr_ch (PieceMgr.PieceReceived (index, c.buffer));
        request_new_piece self
      end else
        request_more_blocks self
    end

let peer_cancel self index block =
  send self (SendCancel (index, block));
  Lwt.return_unit

let handle_peer_msg self msg =
  self.update_dl (Wire.size_of_msg msg);
  match msg with
  | Wire.KeepAlive                    -> Lwt.return_unit
  | Wire.Choke                        -> peer_choke self
  | Wire.Unchoke                      -> peer_unchoke self
  | Wire.Interested                   -> peer_interested self
  | Wire.NotInterested                -> peer_not_interested self
  | Wire.Have index                   -> peer_have self index
  | Wire.BitField bits                -> peer_bitfield self bits
  | Wire.Request (index, block)       -> peer_request self index block
  | Wire.Piece (index, offset, block) -> peer_piece self index offset block
  | Wire.Cancel (index, block)        -> peer_cancel self index block
  | msg -> debug self.id "Unahandled: %s" (Wire.string_of_msg msg)

let handle_timer_tick self =
  let keep_alive () =
    if self.last_msg >= 24 then
      send self (SendMsg Wire.KeepAlive)
    else
      self.last_msg <- self.last_msg + 1
  in
  keep_alive ();
  Lwt_pipe.write self.choke_mgr_ch
    (ChokeMgr.PeerRateUpdate (self.id, Lwt_react.S.value self.ul,
      Lwt_react.S.value self.dl))
  (* later- tell Status, tell ChokeMgr *)

let handle_message self msg : unit Lwt.t =
  (* debug self.id "%s" (string_of_msg msg) >>= fun () -> *)
  match msg with
  | PeerMsg msg ->
    handle_peer_msg self msg
  | Tick ->
    handle_timer_tick self;
    Lwt.return_unit
  | PieceCompleted index ->
    send self (SendMsg (Wire.Have index));
    Lwt.return_unit
  | ReceiverAborted _
  | SenderAborted _ ->
    raise_lwt Exit
  | BytesSent sz ->
    self.update_ul sz;
    Lwt.return_unit
  | Choke ->
    debug self.id "Choking %s" (to_string self) >>= fun () ->
    self.we_choking <- true;
    Lwt_pipe.write self.choke_mgr_ch (ChokeMgr.PeerChoked self.id);
    Lwt.return_unit
  | UnChoke ->
    debug self.id "Unchoking %s" (to_string self) >>= fun () ->
    self.we_choking <- false;
    Lwt_pipe.write self.choke_mgr_ch (ChokeMgr.PeerUnChoked self.id);
    Lwt.return_unit
  (* | msg -> *)
  (*   debug t.id "Unhandled: %s" (string_of_msg msg) *)

let get_done self =
  let mv = Lwt_mvar.create_empty () in
  Lwt_pipe.write self.piece_mgr_ch (PieceMgr.GetDone mv);
  Lwt_mvar.take mv

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

let start addr port peer_id ic oc ~peer_mgr_ch ih ~pieces ~piece_mgr_ch ~fs_ch ~choke_mgr_ch =
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
    let self =
      { addr; port; peer_id;
        we_interested = false; we_choking = true;
        peer_interested = false; peer_choking = true;
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
    get_done self >>= fun bits ->
    if Bits.count bits > 0 then send self (SendMsg (Wire.BitField bits));
    try_lwt
      Lwt_pipe.iter_s (handle_message self) ch
    finally
      begin
        Lwt.cancel self.ticker;
        ignore (Lwt_io.close self.ic);
        ignore (Lwt_io.close self.oc);
        Lwt_pipe.write self.peer_mgr_ch (Disconnect id);
        Lwt_pipe.write self.piece_mgr_ch (PieceMgr.PeerUnHave (Bits.to_list self.peer_pieces));
        Lwt_pipe.write self.choke_mgr_ch (ChokeMgr.RemovePeer id);
        match self.current with
        | Some c ->
          Lwt_pipe.write self.piece_mgr_ch (PieceMgr.PutbackPiece c.index);
          Lwt.return_unit
        | None ->
          Lwt.return_unit
      end
  in
  Proc.spawn ~name:"Peer" run
