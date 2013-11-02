open Printf
open Messages
open Monitor

(* let event_loop id oc v = *)
(*   let rec loop () = *)
(*     lwt s = Lwt_mvar.take v in *)
(*     Lwt_io.write oc s >> loop () *)
(*   in loop () *)
(*  *)
(* let start ~monitor oc v = *)
(*   Monitor.spawn ~parent:monitor ~name:"Sender" (fun id -> event_loop id oc v) *)
(*   (* Supervisor.spawn "Sender" sup_ch (fun id -> event_loop id oc v) *) *)

let (>>=) = Lwt.(>>=)

let string_of_msg = function
  | SendMsg msg ->
    sprintf "SendMsg: %s" (string_of_peer_msg msg)
  | SendPiece (index, Block (offset, length)) ->
    sprintf "SendPiece: index: %d offset: %d length: %d" index offset length
  | SendCancel (index, Block (offset, length)) ->
    sprintf "SendCancel: index: %d offset: %d length: %d"
      index offset length

let write_int8 oc n : unit Lwt.t =
  Lwt_io.write_char oc (char_of_int n)

(**
@returns total number of bytes written *)
let send_msg id oc msg : int Lwt.t =
  match msg with
  | KeepAlive ->
    Lwt_io.BE.write_int oc 0 >>
    Lwt.return 4
  | Choke ->
    Lwt_io.BE.write_int oc 1 >>
    write_int8 oc 0 >>
    Lwt.return 5
  | Unchoke ->
    Lwt_io.BE.write_int oc 1 >>
    write_int8 oc 1 >>
    Lwt.return 5
  | Interested ->
    Lwt_io.BE.write_int oc 1 >>
    write_int8 oc 2 >>
    Lwt.return 5
  | NotInterested ->
    Lwt_io.BE.write_int oc 1 >>
    write_int8 oc 3 >>
    Lwt.return 5
  | Have index ->
    Lwt_io.BE.write_int oc 5 >>
    write_int8 oc 4 >>
    Lwt_io.BE.write_int oc index >>
    Lwt.return 9
  | BitField bits ->
    let bits = Bits.pad bits 8 in
    let len = Bits.length bits in
    Lwt_io.BE.write_int oc (1 + len) >>
    write_int8 oc 5 >>
    Lwt_io.write oc (Bits.to_bin bits) >>
    Lwt.return (5 + len)
  | Request (index, Block (offset, length)) ->
    Lwt_io.BE.write_int oc 13 >>
    write_int8 oc 6 >>
    Lwt_io.BE.write_int oc index >>
    Lwt_io.BE.write_int oc offset >>
    Lwt_io.BE.write_int oc length >>
    Lwt.return 17
  | Piece (index, offset, block) ->
    let len = String.length block in
    Lwt_io.BE.write_int oc (9 + len) >>
    write_int8 oc 7 >>
    Lwt_io.BE.write_int oc index >>
    Lwt_io.BE.write_int oc offset >>
    Lwt_io.write oc block >>
    Lwt.return (13 + len)
  | Cancel (index, Block (offset, length)) ->
    Lwt_io.BE.write_int oc 13 >>
    write_int8 oc 8 >>
    Lwt_io.BE.write_int oc index >>
    Lwt_io.BE.write_int oc offset >>
    Lwt_io.BE.write_int oc length >>
    Lwt.return 17
  | Port port ->
    Lwt_io.BE.write_int oc 3 >>
    write_int8 oc 9 >>
    Lwt_io.BE.write_int16 oc port >>
    Lwt.return 7

let handle_message id oc send_peer_mgr msg =
  debug id "%s" (string_of_msg msg) >>
  match msg with
  | SendMsg msg ->
    lwt sz = send_msg id oc msg in
    send_peer_mgr (FromSender sz);
    Lwt.return ()
  | msg ->
    debug id "unhandled message: %s" (string_of_msg msg)

let start ~monitor oc sender_ch send_peer_mgr =
  let event_loop id =
    Lwt_stream.iter_s (handle_message id oc send_peer_mgr) sender_ch
  in
  Monitor.spawn ~parent:monitor ~name:"Sender" event_loop
