open Printf

type block =
  | Block of int * int

type msg =
  | KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have of int
  | BitField of Bits.t
  | Request of int * block
  | Piece of int * int * string
  | Cancel of int * block
  | Port of int

let string_of_msg = function
  | KeepAlive ->
    "KeepAlive"
  | Choke ->
    "Choke"
  | Unchoke ->
    "Unchoke"
  | Interested ->
    "Interested"
  | NotInterested ->
    "NotInterested"
  | Have index ->
    sprintf "Have: index: %d" index
  | BitField bits ->
    sprintf "BitField: length: %d" (Bits.length bits)
  | Request (index, Block (offset, length)) ->
    sprintf "Request: index: %d offset: %d length: %d" index offset length
  | Piece (index, offset, _) ->
    sprintf "Piece: index: %d offset: %d" index offset
  | Cancel (index, Block (offset, length)) ->
    sprintf "Cancel: index: %d offset: %d length: %d" index offset length
  | Port port ->
    sprintf "Port: %d" port

let size_of_msg = function
  | KeepAlive -> 4
  | Choke
  | Unchoke
  | Interested
  | NotInterested -> 5
  | Have _ -> 9
  | BitField bits -> 5 + Bits.length (Bits.pad bits 8)
  | Request _ -> 17
  | Piece (_, _, block) -> String.length block + 13
  | Cancel _ -> 17
  | Port _ -> 7

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let write_int8 oc n : unit Lwt.t =
  Lwt_io.write_char oc (char_of_int n)

let write oc msg : int Lwt.t =
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

let read_int8 ic =
  Lwt_io.read_char ic >>= fun ch ->
  Lwt.return (int_of_char ch)

let read_exactly ic len =
  let buf = String.create len in
  Lwt_io.read_into_exactly ic buf 0 len >>= fun () ->
  Lwt.return buf

exception BadMsg of int * int

let read_msg ic len msgid =
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

let read ic =
  (* this is safe in 64-bit *)
  lwt len = Lwt_io.BE.read_int ic in
  if len = 0 then
    Lwt.return KeepAlive
  else
    read_int8 ic >>= read_msg ic len
