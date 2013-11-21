open Printf
open Msg

let (>>=) = Lwt.(>>=)

let debug = Proc.debug

let string_of_msg = function
  | SendMsg msg ->
    sprintf "SendMsg: %s" (Wire.string_of_msg msg)
  | SendPiece (index, Wire.Block (offset, length)) ->
    sprintf "SendPiece: index: %d offset: %d length: %d" index offset length
  | SendCancel (index, Wire.Block (offset, length)) ->
    sprintf "SendCancel: index: %d offset: %d length: %d"
      index offset length

let handle_message id oc peer_ch msg =
  debug id "%s" (string_of_msg msg) >>
  match msg with
  | SendMsg msg ->
    Wire.write oc msg >>= fun sz ->
    Lwt_pipe.write peer_ch (BytesSent sz);
    Lwt.return_unit
  | msg ->
    debug id "Unhandled: %s" (string_of_msg msg)

let start oc ~ch ~peer_ch =
  let run id =
    try_lwt
      Lwt_pipe.iter_s (handle_message id oc peer_ch) ch
    with
    | exn ->
      Lwt_pipe.write peer_ch (Msg.SenderAborted exn);
      Lwt.return_unit
    finally
      Lwt_io.close oc
  in
  Proc.spawn ~name:"Sender" run (fun _ -> Lwt.return_unit)
