open Msg

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let read_int8 ic =
  lwt ch = Lwt_io.read_char ic in
  Lwt.return (int_of_char ch)

let read_exactly ic len =
  let buf = String.create len in
  Lwt_io.read_into_exactly ic buf 0 len >>
  Lwt.return buf

exception BadMsg of int * int

let read_msg id ic len msgid =
  match msgid, len with
  | 0, 1 -> Lwt.return Choke
  | 1, 1 -> Lwt.return Unchoke
  | 2, 1 -> Lwt.return Interested
  | 3, 1 -> Lwt.return NotInterested
  | 4, 5 ->
    lwt index = Lwt_io.BE.read_int ic in
    Lwt.return (Have index)
  | 5, _ ->
    let len' = len-1 in
    lwt buf = read_exactly ic len' in
    Lwt.return (BitField (Bits.of_bin buf))
  | 6, 13 ->
    lwt index = Lwt_io.BE.read_int ic in
    lwt start = Lwt_io.BE.read_int ic in
    lwt lengt = Lwt_io.BE.read_int ic in
    Lwt.return (Request (index, Block (start, lengt)))
  | 7, _ ->
    let len' = len-9 in
    lwt index = Lwt_io.BE.read_int ic in
    lwt start = Lwt_io.BE.read_int ic in
    lwt block = read_exactly ic len' in
    Lwt.return (Piece (index, start, block))
  | 8, 13 ->
    lwt index = Lwt_io.BE.read_int ic in
    lwt start = Lwt_io.BE.read_int ic in
    lwt lengt = Lwt_io.BE.read_int ic in
    Lwt.return (Cancel (index, Block (start, lengt)))
  | 9, 3 ->
    lwt port = Lwt_io.BE.read_int16 ic in
    Lwt.return (Port port)
  | _ ->
    raise_lwt (BadMsg (msgid, len))

let stream_of_channel id (ic : Lwt_io.input_channel) : peer_msg Lwt_stream.t =
  let read () =
    (* this is safe in 64-bit *)
    lwt len = Lwt_io.BE.read_int ic in
    if len = 0 then
      (* keep-alive; ignored; in the future we should drop the connction if
         two minutes elapse with no activity *)
      Lwt.return KeepAlive
    else
      read_int8 ic >>= read_msg id ic len
  in
  Lwt_stream.from (fun () -> lwt msg = read () in Lwt.return (Some msg))

let start ~send_super ic ~send_peer =
  let run id =
    (* let t = { send_peer; id } in *)
    Lwt_stream.iter (fun msg -> send_peer (Some (FromPeer msg)))
      (stream_of_channel id ic)
  in
  Proc.spawn (Proc.cleanup run
    (Super.default_stop send_super) (fun _ -> Lwt.return_unit))
