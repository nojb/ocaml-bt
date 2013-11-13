open Msg
open Printf

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug id ?exn fmt =
  Printf.ksprintf (fun msg ->
    Lwt_log.debug_f ?exn "Peer %s: %s" (Proc.Id.to_string id) msg) fmt

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

type t = {
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
  send_peer_mgr : Msg.peer_mgr_msg option -> unit;
  send_sender : Msg.sender_msg option -> unit;
  send_piece_mgr : Msg.piece_mgr_msg option -> unit;
  id : Proc.Id.t
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

let send_sender t msg =
  t.last_msg <- 0;
  t.send_sender (Some msg)

let grab_blocks t n : (int * block) list Lwt.t =
  let mv = Lwt_mvar.create_empty () in
  t.send_piece_mgr (Some (GrabBlocks (n, t.peer_pieces, mv)));
  Lwt_mvar.take mv

let fill_blocks t =
  if not t.peer_choking && t.we_interested then
    let sz = BlockSet.cardinal t.block_queue in
    if sz < lo_mark then begin
      lwt blks = grab_blocks t (hi_mark - sz) in
      List.iter (fun (pn, b) ->
        if not (BlockSet.mem (pn, b) t.block_queue) then
          send_sender t (SendMsg (Request (pn, b)))) blks;
      t.block_queue <-
        List.fold_left (fun bq b -> BlockSet.add b bq) t.block_queue blks;
      Lwt.return_unit
    end else
      Lwt.return_unit
  else
    Lwt.return_unit

exception UnknownPiece of int

let handle_peer_msg t = function
  | KeepAlive ->
    Lwt.return_unit
  | Choke ->
    t.send_piece_mgr (Some (PutbackBlocks (BlockSet.elements t.block_queue)));
    t.block_queue <- BlockSet.empty;
    t.peer_choking <- true;
    Lwt.return_unit
  | Unchoke ->
    t.peer_choking <- false;
    fill_blocks t
  | Interested ->
    t.peer_interested <- true;
    Lwt.return_unit
  | NotInterested ->
    t.peer_interested <- false;
    Lwt.return_unit
  | Have index ->
    if index >= 0 && index < Array.length t.pieces then begin
      Bits.set t.peer_pieces index;
      (* later : track interest, decr missing counter *)
      fill_blocks t
    end else
      raise_lwt (UnknownPiece index)
  | BitField bits ->
    if Bits.count t.peer_pieces = 0 then begin
      t.peer_pieces <- bits;
      Lwt.return_unit
    end else
      Lwt.return_unit
  | Request (index, block) ->
    if not t.we_choking then send_sender t (SendPiece (index, block));
    Lwt.return_unit
  | Piece (index, offset, block) ->
    let blk = Block (offset, String.length block) in
    if BlockSet.mem (index, blk) t.block_queue then begin
      (* FIXME FIXME Store Block Here *)
      t.block_queue <- BlockSet.remove (index, blk) t.block_queue;
      Lwt.return ()
    end else
      debug t.id "Received unregistered piece: index: %d offset: %d length: %d"
        index offset (String.length block)
  | Cancel (index, block) ->
    send_sender t (SendCancel (index, block));
    Lwt.return ()
  | msg ->
    debug t.id "Unahandled: %s" (string_of_peer_msg msg)

let handle_sender_msg t sz =
  (* later: update some rates *)
  Lwt.return_unit

let handle_timer_tick t =
  let keep_alive () =
    if t.last_msg >= 24 then
      send_sender t (SendMsg KeepAlive)
    else
      t.last_msg <- t.last_msg + 1
  in
  keep_alive ()
  (* later- tell Status, tell ChokeMgr *)

let handle_message t msg : unit Lwt.t =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | FromPeer msg ->
    handle_peer_msg t msg
  | FromSender sz ->
    handle_sender_msg t sz
  | FromTimer ->
    handle_timer_tick t;
    Lwt.return_unit

let start_peer ~send_super ~send_peer_mgr msgs ih ~pieces
  ~send_sender ~send_piece_mgr =
  let run id =
    let t =
      { we_interested = true;
        we_choking = false;
        block_queue = BlockSet.empty;
        peer_interested = true;
        peer_choking = false;
        peer_pieces = Bits.create (Array.length pieces);
        pieces;
        send_sender;
        send_piece_mgr;
        send_peer_mgr;
        last_msg = 0;
        id }
    in
    t.send_peer_mgr (Some (Connect (ih, id)));
    Lwt_stream.iter_s (handle_message t) msgs
  in
  Proc.spawn (Proc.cleanup run
    (Super.default_stop send_super) (fun _ -> Lwt.return_unit))

let start ic oc ~send_peer_mgr ih ~pieces ~send_piece_mgr =
  let msgs_sender, send_sender = Lwt_stream.create () in
  let msgs, send = Lwt_stream.create () in
  [
    Worker (Sender.start oc msgs_sender ~send_peer:send);
    Worker (Receiver.start ic ~send_peer:send);
    Worker (start_peer ~send_peer_mgr msgs ih ~pieces ~send_sender ~send_piece_mgr)
  ]
