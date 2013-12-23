open Printf

type message =
  | KEEP_ALIVE
  | CHOKE
  | UNCHOKE
  | INTERESTED
  | NOT_INTERESTED
  | HAVE of int
  | BITFIELD of Bits.t
  | REQUEST of int * int * int
  | PIECE of int * int * string
  | CANCEL of int * int * int
  | PORT of int
  | EXTENDED of int * string

let string_of_message = function
  | KEEP_ALIVE ->
    "KEEP_ALIVE"
  | CHOKE ->
    "CHOKE"
  | UNCHOKE ->
    "UNCHOKE"
  | INTERESTED ->
    "INTERESTED"
  | NOT_INTERESTED ->
    "NOT_INTERESTED"
  | HAVE i ->
    sprintf "HAVE %d" i
  | BITFIELD b ->
    sprintf "BITFIELD count: %d" (Bits.count b)
  | REQUEST (i, off, len) ->
    sprintf "REQUEST %d offset: %d length: %d" i off len
  | PIECE (i, off, _) ->
    sprintf "PIECE %d offset: %d" i off
  | CANCEL (i, off, len) ->
    sprintf "CANCEL %d offset: %d length: %d" i off len
  | PORT port ->
    sprintf "PORT %d" port
  | EXTENDED (id, _) ->
    sprintf "EXTENDED %d" id

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let put' msg : Put.t =
  let open Put in
  let open Put.BE in
  match msg with
  | KEEP_ALIVE ->
    string ""
  | CHOKE ->
    int8 0
  | UNCHOKE ->
    int8 1
  | INTERESTED ->
    int8 2
  | NOT_INTERESTED ->
    int8 3
  | HAVE i ->
    int8 4 >> int i
  | BITFIELD b ->
    int8 5 >> string (Bits.pad b 8 |> Bits.to_bin)
  | REQUEST (i, off, len) ->
    int8 6 >> int i >> int off >> int len
  | PIECE (i, off, s) ->
    int8 7 >> int i >> int off >> string s
  | CANCEL (i, off, len) ->
    int8 8 >> int i >> int off >> int len
  | PORT i ->
    int8 9 >> int16 i
  | EXTENDED (id, s) ->
    int8 20 >> int8 id >> string s

let put msg =
  let p = put' msg in
  Put.(BE.int (length p) >> p)

(* let write oc msg = *)
(*   put msg |> Put.run |> Lwt_io.write oc *)
    
let read_exactly ic len =
  let buf = String.create len in
  Lwt_io.read_into_exactly ic buf 0 len >>= fun () ->
  Lwt.return buf

exception BadMsg of int * int

let get' len id : message Get.t =
  let open Get in
  let open Get.BE in
  match id with
  | 0 ->
    return CHOKE
  | 1 ->
    return UNCHOKE
  | 2 ->
    return INTERESTED
  | 3 ->
    return NOT_INTERESTED
  | 4 ->
    int >>= fun i ->
    return (HAVE i)
  | 5 ->
    string_of_length (len-1) >>= fun s ->
    return (BITFIELD (Bits.of_bin s))
  | 6 ->
    int >>= fun i ->
    int >>= fun off ->
    int >>= fun len ->
    return (REQUEST (i, off, len))
  | 7 ->
    int >>= fun i ->
    int >>= fun off ->
    string_of_length (len-9) >>= fun s ->
    return (PIECE (i, off, s))
  | 8 ->
    int >>= fun i ->
    int >>= fun off ->
    int >>= fun len ->
    return (CANCEL (i, off, len))
  | 9 ->
    uint16 >>= fun port ->
    return (PORT port)
  | 20 ->
    uint8 >>= fun id ->
    string_of_length (len-2) >>= fun s ->
    return (EXTENDED (id, s))
  | _ ->
    fail (* fail (BadMsg (len, id)) *)

let get len : message Get.t =
  if len = 0 then
    Get.return KEEP_ALIVE
  else
    Get.(BE.uint8 >>= get' len)

let read ic =
  Lwt_io.BE.read_int ic >>= fun len ->
  read_exactly ic len >|= Get.run (get len)
