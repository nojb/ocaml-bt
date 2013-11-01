open Messages
open Printf
open Monitor

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let create_stream () =
  let s, w = Lwt_stream.create () in
  s, (fun x -> w (Some x))

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

(* type state = { *)
(*   we_interested : bool; *)
(*   we_choking : bool; *)
(*   block_queue : BlockSet.t; *)
(*   peer_choking : bool; *)
(*   peer_interested : bool; *)
(*   peer_pieces : PieceSet.t; *)
(*   missing_pieces : int; *)
(*   up_rate : rate; *)
(*   down_rate : rate; *)
(*   last_msg_tick : int; *)
(*   last_piece_tick : int; *)
(*   interesting_pieces : PieceSet.t; *)
(*   last_pn : int *)
(* } *)

let string_of_msg = function
  | FromPeer msg ->
    sprintf "FromPeer: %s" (string_of_peer_msg msg)
  | FromSender sz ->
    sprintf "FromSender: %d bytes sent" sz

let start_peer ~monitor w_pmc in_ch ih =
  let startup id =
    w_pmc (Connect (ih, id));
    Lwt.return ()
  in
  let event_loop id =
    Lwt_stream.iter_s (fun msg -> debug id "%s" (string_of_msg msg)) in_ch
  in
  (* Add a cleanup operation *)
  Monitor.spawn ~parent:monitor ~name:"Peer"
    (fun id -> startup id >> event_loop id)

let start ~monitor ic oc w_pmc ih =
  (* let senderv = Lwt_mvar.create_empty () in *)
  let sender_ch, _ = create_stream () in
  let in_ch, w_in = create_stream () in
  let monitor = Monitor.create ~parent:monitor Monitor.AllForOne "PeerSup"
  in
  Sender.start ~monitor oc sender_ch w_in;
  Receiver.start ~monitor ic w_in;
  start_peer ~monitor w_pmc in_ch ih
