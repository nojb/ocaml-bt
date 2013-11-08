open Messages
open Printf

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug = Supervisor.debug

let create_stream () =
  let s, w = Lwt_stream.create () in
  s, (fun x -> w (Some x))

let string_of_msg = function
  | FromPeer msg ->
    sprintf "FromPeer: %s" (string_of_peer_msg msg)
  | FromSender sz ->
    sprintf "From Sender: %d" sz
  | FromTimer ->
    "FromTimer"

(* let string_of_peer_msg = function *)
(*   | KeepAlive -> *)
(*     "KeepAlive" *)
(*   | Choke -> *)
(*     "Choke" *)
(*   | Unchoke -> *)
(*     "Unchoke" *)
(*   | Interested -> *)
(*     "Interested" *)
(*   | NotInterested -> *)
(*     "NotInterested" *)
(*   | Have index -> *)
(*     sprintf "Have: index: %d" index *)
(*   | BitField bits -> *)
(*     sprintf "BitField: length: %d" (Bits.length bits) *)
(*   | Request (index, Block (offset, length)) -> *)
(*     sprintf "Request: index: %d offset: %d length: %d" index offset length *)
(*   | Piece (index, offset, _) -> *)
(*     sprintf "Piece: index: %d offset: %d" index offset *)
(*   | Cancel (index, Block (offset, length)) -> *)
(*     sprintf "Cancel: index: %d offset: %d length: %d" index offset length *)
(*   | Port port -> *)
(*     sprintf "Port: %d" port *)

module Block = struct
  type t = int * block
  let compare = compare
end

module BlockSet = Set.Make(Block)

type state = {
  mutable we_interested : bool;
  mutable we_choking : bool;
  mutable block_queue : BlockSet.t;
  mutable peer_interested : bool;
  mutable peer_choking : bool;
  mutable peer_pieces : Bits.t;
  mutable last_msg : int;
  (* missing_pieces : int; *)
  (* up_rate : rate; *)
  (* down_rate : rate; *)
  (* last_msg_tick : int; *)
  (* last_piece_tick : int; *)
  (* interesting_pieces : PieceSet.t; *)
  (* mutable last_pn : int; *)
  pieces : Torrent.piece_info array;
  msg_sender : sender_msg -> unit;
  msg_piece_mgr : piece_mgr_msg -> unit
}

let string_of_msg = function
  | FromPeer msg ->
    sprintf "FromPeer: %s" (string_of_peer_msg msg)
  | FromSender sz ->
    sprintf "FromSender: %d bytes sent" sz
  | FromTimer ->
    "FromTimer"

let lo_mark = 5
let hi_mark = 25

let send id st msg =
  st.last_msg <- 0;
  st.msg_sender msg

let grab_blocks id st n : (int * block) list Lwt.t =
  let mv = Lwt_mvar.create_empty () in
  st.msg_piece_mgr (GrabBlocks (n, st.peer_pieces, mv));
  Lwt_mvar.take mv

let fill_blocks id st =
  if not st.peer_choking && st.we_interested then
    let sz = BlockSet.cardinal st.block_queue in
    if sz < lo_mark then begin
      lwt blks = grab_blocks id st (hi_mark - sz) in
      List.iter (fun (pn, b) ->
        if not (BlockSet.mem (pn, b) st.block_queue) then
          send id st (SendMsg (Request (pn, b)))) blks;
      st.block_queue <-
        List.fold_left (fun bq b -> BlockSet.add b bq) st.block_queue blks;
      Lwt.return ()
    end else
      Lwt.return ()
  else
    Lwt.return ()


exception UnknownPiece of int

let handle_peer_msg id st = function
  | KeepAlive ->
    Lwt.return ()
  | Choke ->
    st.msg_piece_mgr (PutbackBlocks (BlockSet.elements st.block_queue));
    st.block_queue <- BlockSet.empty;
    st.peer_choking <- true;
    Lwt.return ()
  | Unchoke ->
    st.peer_choking <- false;
    fill_blocks id st
  | Interested ->
    st.peer_interested <- true;
    Lwt.return ()
  | NotInterested ->
    st.peer_interested <- false;
    Lwt.return ()
  | Have index ->
    if index >= 0 && index < Array.length st.pieces then begin
      Bits.set st.peer_pieces index;
      (* later : track interest, decr missing counter *)
      fill_blocks id st
    end else
      raise_lwt (UnknownPiece index)
  | BitField bits ->
    if Bits.count st.peer_pieces = 0 then begin
      st.peer_pieces <- bits;
      Lwt.return ()
    end else
      Lwt.return ()
  | Request (index, block) ->
    if not st.we_choking then send id st (SendPiece (index, block));
    Lwt.return ()
  | Piece (index, offset, block) ->
    let blk = Block (offset, String.length block) in
    if BlockSet.mem (index, blk) st.block_queue then begin
      (* FIXME FIXME Store Block Here *)
      st.block_queue <- BlockSet.remove (index, blk) st.block_queue;
      Lwt.return ()
    end else
      debug id "Received unregistered piece: index: %d offset: %d length: %d"
        index offset (String.length block)
  | Cancel (index, block) ->
    send id st (SendCancel (index, block));
    Lwt.return ()
  | msg ->
    debug id "Unahandled: %s" (string_of_peer_msg msg)

let handle_sender_msg id st sz =
  (* later: update some rates *)
  Lwt.return ()

let handle_timer_tick id st =
  let keep_alive () =
    if st.last_msg >= 24 then
      send id st (SendMsg KeepAlive)
    else
      st.last_msg <- st.last_msg + 1
  in
  keep_alive ()
  (* later- tell Status, tell ChokeMgr *)

let handle_msg id st msg : unit Lwt.t =
  debug id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | FromPeer msg ->
    handle_peer_msg id st msg
  | FromSender sz ->
    handle_sender_msg id st sz
  | FromTimer ->
    handle_timer_tick id st;
    Lwt.return ()

let start_peer ~msg_supervisor w_pmc in_ch ih ~pieces ~msg_sender ~msg_piece_mgr =
  let st =
    { we_interested = true;
      we_choking = false;
      block_queue = BlockSet.empty;
      peer_interested = true;
      peer_choking = false;
      peer_pieces = Bits.create (Array.length pieces);
      pieces;
      msg_sender;
      msg_piece_mgr;
      last_msg = 0 }
  in
  let startup id =
    w_pmc (Connect (ih, id));
    Lwt.return ()
  in
  let event_loop id =
    Lwt_stream.iter_s (handle_msg id st) in_ch
  in
  (* Add a cleanup operation *)
  Supervisor.spawn_worker msg_supervisor "Peer"
    (fun id -> startup id >> event_loop id)

let start ~msg_supervisor ic oc w_pmc ih ~pieces ~msg_piece_mgr =
  let sender_ch, msg_sender = create_stream () in
  let in_ch, w_in = create_stream () in
  let msg_peer_sup = Supervisor.spawn_supervisor msg_supervisor "PeerSup"
    Supervisor.AllForOne
  in
  Sender.start ~msg_supervisor:msg_peer_sup oc sender_ch w_in;
  Receiver.start ~msg_supervisor:msg_peer_sup ic w_in;
  start_peer ~msg_supervisor:msg_peer_sup w_pmc in_ch ih ~pieces ~msg_sender ~msg_piece_mgr
