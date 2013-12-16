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
  | Extended of int * string

let string_of_msg = function
  | KeepAlive ->
    "KEEPALIVE"
  | Choke ->
    "CHOKE"
  | Unchoke ->
    "UNCHOKE"
  | Interested ->
    "INTERESTED"
  | NotInterested ->
    "NOTINTERESTED"
  | Have index ->
    sprintf "HAVE %d" index
  | BitField bits ->
    sprintf "BITFIELD count: %d" (Bits.count bits)
  | Request (index, Block (offset, length)) ->
    sprintf "REQUEST %d offset: %d length: %d" index offset length
  | Piece (index, offset, _) ->
    sprintf "PIECE %d offset: %d" index offset
  | Cancel (index, Block (offset, length)) ->
    sprintf "CANCEL %d offset: %d length: %d" index offset length
  | Port port ->
    sprintf "PORT %d" port
  | Extended (id, _) ->
    sprintf "EXTENDED %d" id

(* let size_of_msg = function *)
(*   | KeepAlive -> 4 *)
(*   | Choke *)
(*   | Unchoke *)
(*   | Interested *)
(*   | NotInterested -> 5 *)
(*   | Have _ -> 9 *)
(*   | BitField bits -> 5 + Bits.length (Bits.pad bits 8) *)
(*   | Request _ -> 17 *)
(*   | Piece (_, _, block) -> String.length block + 13 *)
(*   | Cancel _ -> 17 *)
(*   | Port _ -> 7 *)
(*   | Extended (_, s) -> 6 + String.length s *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let write_int8 oc n =
  Lwt_io.write_char oc (char_of_int n)

let write oc msg =
  match msg with
  | KeepAlive ->
    Lwt_io.BE.write_int oc 0 >>= fun () ->
    Lwt.return 4
  | Choke ->
    Lwt_io.BE.write_int oc 1 >>= fun () ->
    write_int8 oc 0 >>= fun () ->
    Lwt.return 5
  | Unchoke ->
    Lwt_io.BE.write_int oc 1 >>= fun () ->
    write_int8 oc 1 >>= fun () ->
    Lwt.return 5
  | Interested ->
    Lwt_io.BE.write_int oc 1 >>= fun () ->
    write_int8 oc 2 >>= fun () ->
    Lwt.return 5
  | NotInterested ->
    Lwt_io.BE.write_int oc 1 >>= fun () ->
    write_int8 oc 3 >>= fun () ->
    Lwt.return 5
  | Have index ->
    Lwt_io.BE.write_int oc 5 >>= fun () ->
    write_int8 oc 4 >>= fun () ->
    Lwt_io.BE.write_int oc index >>= fun () ->
    Lwt.return 9
  | BitField bits ->
    let bits = Bits.pad bits 8 in
    let len = Bits.length bits in
    Lwt_io.BE.write_int oc (1 + len) >>= fun () ->
    write_int8 oc 5 >>= fun () ->
    Lwt_io.write oc (Bits.to_bin bits) >>= fun () ->
    Lwt.return (5 + len)
  | Request (index, Block (offset, length)) ->
    Lwt_io.BE.write_int oc 13 >>= fun () ->
    write_int8 oc 6 >>= fun () ->
    Lwt_io.BE.write_int oc index >>= fun () ->
    Lwt_io.BE.write_int oc offset >>= fun () ->
    Lwt_io.BE.write_int oc length >>= fun () ->
    Lwt.return 17
  | Piece (index, offset, block) ->
    let len = String.length block in
    Lwt_io.BE.write_int oc (9 + len) >>= fun () ->
    write_int8 oc 7 >>= fun () ->
    Lwt_io.BE.write_int oc index >>= fun () ->
    Lwt_io.BE.write_int oc offset >>= fun () ->
    Lwt_io.write oc block >>= fun () ->
    Lwt.return (13 + len)
  | Cancel (index, Block (offset, length)) ->
    Lwt_io.BE.write_int oc 13 >>= fun () ->
    write_int8 oc 8 >>= fun () ->
    Lwt_io.BE.write_int oc index >>= fun () ->
    Lwt_io.BE.write_int oc offset >>= fun () ->
    Lwt_io.BE.write_int oc length >>= fun () ->
    Lwt.return 17
  | Port port ->
    Lwt_io.BE.write_int oc 3 >>= fun () ->
    write_int8 oc 9 >>= fun () ->
    Lwt_io.BE.write_int16 oc port >>= fun () ->
    Lwt.return 7
  | Extended (id, s) ->
    Lwt_io.BE.write_int oc (String.length s + 2) >>= fun () ->
    Lwt_io.BE.write_int oc 20 >>= fun () ->
    write_int8 oc id >>= fun () ->
    Lwt_io.write oc s >>= fun () ->
    Lwt.return (String.length s + 6)

let read_int8 ic =
  Lwt_io.read_char ic >|= int_of_char
    
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
    Lwt_io.BE.read_int ic >|= fun index ->
    Have index
  | 5, _ ->
    let len' = len-1 in
    read_exactly ic len' >|= fun buf -> BitField (Bits.of_bin buf)
  | 6, 13 ->
    Lwt_io.BE.read_int ic >>= fun index ->
    Lwt_io.BE.read_int ic >>= fun start ->
    Lwt_io.BE.read_int ic >|= fun lengt ->
    Request (index, Block (start, lengt))
  | 7, _ ->
    let len' = len-9 in
    Lwt_io.BE.read_int ic >>= fun index ->
    Lwt_io.BE.read_int ic >>= fun start ->
    read_exactly ic len' >|= fun block ->
    Piece (index, start, block)
  | 8, 13 ->
    Lwt_io.BE.read_int ic >>= fun index ->
    Lwt_io.BE.read_int ic >>= fun start ->
    Lwt_io.BE.read_int ic >|= fun lengt ->
    Cancel (index, Block (start, lengt))
  | 9, 3 ->
    Lwt_io.BE.read_int16 ic >|= fun port -> Port port
  | 20, _ ->
    let len = len-2 in
    read_int8 ic >>= fun id ->
    read_exactly ic len >|= fun s ->
    Extended (id, s)
  | _ ->
    Lwt.fail (BadMsg (msgid, len))

let read ic =
  (* this is safe in 64-bit *)
  Lwt_io.BE.read_int ic >>= fun len ->
  if len = 0 then
    Lwt.return KeepAlive
  else
    read_int8 ic >>= read_msg ic len
