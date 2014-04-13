(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

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
  | HAVE_ALL
  | HAVE_NONE
  | SUGGEST of int
  | REJECT of int * int * int
  | ALLOWED of int list
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
  | HAVE_ALL ->
    "HAVE_ALL"
  | HAVE_NONE ->
    "HAVE_NONE"
  | SUGGEST i ->
    sprintf "SUGGEST_PIECE %d" i
  | REJECT (i, off, len) ->
    sprintf "REJECT_PIECE %d off: %d len: %d" i off len
  | ALLOWED pieces ->
    sprintf "ALLOWED_FAST %s" (String.concat " " (List.map string_of_int pieces))
  | EXTENDED (id, _) ->
    sprintf "EXTENDED %d" id

let sprint m () =
  string_of_message m

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
  | BITFIELD bits ->
    int8 5 >> string (Bits.to_bin bits)
  | REQUEST (i, off, len) ->
    int8 6 >> int i >> int off >> int len
  | PIECE (i, off, s) ->
    int8 7 >> int i >> int off >> string s
  | CANCEL (i, off, len) ->
    int8 8 >> int i >> int off >> int len
  | PORT i ->
    int8 9 >> int16 i
  | HAVE_ALL ->
    int8 14
  | HAVE_NONE ->
    int8 15
  | SUGGEST i ->
    int8 13 >> int i
  | REJECT (i, off, len) ->
    int8 16 >> int i >> int off >> int len
  | ALLOWED pieces ->
    List.fold_left (fun acc i -> acc >> int i) (int8 17) pieces
  | EXTENDED (id, s) ->
    int8 20 >> int8 id >> string s

let put msg =
  let p = put' msg in
  Put.(BE.int (length p) >> p)

(* let write oc msg = *)
(*   put msg |> Put.run |> Lwt_io.write oc *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

(* let read_exactly ic len = *)
(*   let buf = String.create len in *)
(*   Lwt_io.read_into_exactly ic buf 0 len >>= fun () -> *)
(*   Lwt.return buf *)

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
  | 14 ->
    return HAVE_ALL
  | 15 ->
    return HAVE_NONE
  | 13 ->
    int >>= fun i ->
    return (SUGGEST i)
  | 16 ->
    int >>= fun i ->
    int >>= fun off ->
    int >>= fun len ->
    return (REJECT (i, off, len))
  | 17 ->
    many int >>= fun pieces ->
    return (ALLOWED pieces)
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

let memo_get =
  let h = Hashtbl.create 17 in
  fun len ->
    try Hashtbl.find h len
    with Not_found ->
      let g = get len in
      Hashtbl.add h len g;
      g

let get = memo_get

(* let read ic = *)
(*   Lwt_io.BE.read_int ic >>= fun len -> *)
(*   read_exactly ic len >|= Get.run (get len) *)

(* let read_exactly fd len = *)
(*   if len < 0 then invalid_arg "Wire.read_exactly"; *)
(*   let s = String.create len in *)
(*   let rec loop o l = *)
(*     if l <= 0 then *)
(*       Lwt.return s *)
(*     else *)
(*       Lwt_unix.read fd s o l >>= fun l' -> *)
(*       loop (o+l') (l-l') *)
(*   in *)
(*   loop 0 len *)

(* let read_be_int32 fd = *)
(*   read_exactly fd 4 >>= fun s -> *)
(*   Get.run Get.BE.int32 s *)

(* let read_exactly = Util.read_exactly *)

let read sock =
  Tcp.read sock 4 >|= Get.run Get.BE.int >>= fun len ->
  Tcp.read sock len >|= Get.run (get len)

(* type extension = *)
(*   | UT_metadata *)

let lt_extension_bit = 43 (* 20-th bit from the right *)
