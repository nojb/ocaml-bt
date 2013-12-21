open Printf

type block =
  | Block of int * int * int

type msg =
  | KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have of int
  | BitField of Bits.t
  | Request of block
  | Piece of int * int * string
  | Cancel of block
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
  | Request (Block (index, offset, length)) ->
    sprintf "REQUEST %d offset: %d length: %d" index offset length
  | Piece (index, offset, _) ->
    sprintf "PIECE %d offset: %d" index offset
  | Cancel (Block (index, offset, length)) ->
    sprintf "CANCEL %d offset: %d length: %d" index offset length
  | Port port ->
    sprintf "PORT %d" port
  | Extended (id, _) ->
    sprintf "EXTENDED %d" id

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let put' msg : Put.t =
  let open Put in
  let open Put.BE in
  match msg with
  | KeepAlive ->
    string ""
  | Choke ->
    int8 0
  | Unchoke ->
    int8 1
  | Interested ->
    int8 2
  | NotInterested ->
    int8 3
  | Have index ->
    int8 4 >> int index
  | BitField bits ->
    int8 5 >> string (Bits.pad bits 8 |> Bits.to_bin)
  | Request (Block (index, offset, length)) ->
    int8 6 >> int index >> int offset >> int length
  | Piece (index, offset, block) ->
    int8 7 >> int index >> int offset >> string block
  | Cancel (Block (index, offset, length)) ->
    int8 8 >> int index >> int offset >> int length
  | Port port ->
    int8 9 >> int16 port
  | Extended (id, s) ->
    int8 20 >> int8 id >> string s

let put msg =
  let p = put' msg in
  Put.(BE.int (length p) >> p)

let write oc msg =
  put msg |> Put.run |> Lwt_io.write oc
    
let read_exactly ic len =
  let buf = String.create len in
  Lwt_io.read_into_exactly ic buf 0 len >>= fun () ->
  Lwt.return buf

exception BadMsg of int * int

let get' len id : msg Get.t =
  let open Get in
  let open Get.BE in
  match id with
  | 0 ->
    return Choke
  | 1 ->
    return Unchoke
  | 2 ->
    return Interested
  | 3 ->
    return NotInterested
  | 4 ->
    int >>= fun index ->
    return (Have index)
  | 5 ->
    string_of_length (len-1) >>= fun buf ->
    return (BitField (Bits.of_bin buf))
  | 6 ->
    int >>= fun index ->
    int >>= fun start ->
    int >>= fun length ->
    return (Request (Block (index, start, length)))
  | 7 ->
    int >>= fun index ->
    int >>= fun start ->
    string_of_length (len-9) >>= fun block ->
    return (Piece (index, start, block))
  | 8 ->
    int >>= fun index ->
    int >>= fun start ->
    int >>= fun length ->
    return (Cancel (Block (index, start, length)))
  | 9 ->
    uint16 >>= fun port ->
    return (Port port)
  | 20 ->
    uint8 >>= fun id ->
    string_of_length (len-2) >>= fun s ->
    return (Extended (id, s))
  | _ ->
    fail (* fail (BadMsg (len, id)) *)

let get len : msg Get.t =
  if len = 0 then
    Get.return KeepAlive
  else
    Get.(BE.uint8 >>= get' len)

let read ic =
  Lwt_io.BE.read_int ic >>= fun len ->
  read_exactly ic len >|= Get.run (get len)
