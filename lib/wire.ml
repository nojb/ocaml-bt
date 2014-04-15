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
    sprintf "HAVE(i=%d)" i
  | BITFIELD b ->
    sprintf "BITFIELD(count=%d)" (Bits.count b)
  | REQUEST (i, off, len) ->
    sprintf "REQUEST(i=%d,off=%d,len=%d)" i off len
  | PIECE (i, off, _) ->
    sprintf "PIECE(i=%d,off=%d)" i off
  | CANCEL (i, off, len) ->
    sprintf "CANCEL(i=%d,off=%d,len=%d)" i off len
  | PORT port ->
    sprintf "PORT(port=%d)" port
  | HAVE_ALL ->
    "HAVE_ALL"
  | HAVE_NONE ->
    "HAVE_NONE"
  | SUGGEST i ->
    sprintf "SUGGEST_PIECE(i=%d)" i
  | REJECT (i, off, len) ->
    sprintf "REJECT_PIECE(i=%d,off=%d,len=%d)" i off len
  | ALLOWED pieces ->
    sprintf "ALLOWED_FAST(%s)" (String.concat "," (List.map string_of_int pieces))
  | EXTENDED (id, _) ->
    sprintf "EXTENDED(id=%d)" id

let sprint m () =
  string_of_message m

let put' = function
  | KEEP_ALIVE ->
    Bitstring.empty_bitstring
  | CHOKE ->
    BITSTRING { 0 : 8 }
  | UNCHOKE ->
    BITSTRING { 1 : 8 }
  | INTERESTED ->
    BITSTRING { 2 : 8 }
  | NOT_INTERESTED ->
    BITSTRING { 3 : 8 }
  | HAVE i ->
    BITSTRING { 4 : 8; Int32.of_int i : 32 }
  | BITFIELD bits ->
    BITSTRING { 5 : 8; Bits.to_bin bits : -1 : string }
  | REQUEST (i, off, len) ->
    BITSTRING { 6 : 8; Int32.of_int i : 32; Int32.of_int off : 32; Int32.of_int len : 32 }
  | PIECE (i, off, s) ->
    BITSTRING { 7 : 8; Int32.of_int i : 32; Int32.of_int off : 32; s : -1 : string }
  | CANCEL (i, off, len) ->
    BITSTRING { 8 : 8; Int32.of_int i : 32; Int32.of_int off : 32; Int32.of_int len : 32 }
  | PORT i ->
    BITSTRING { 9 : 8; i : 16 }
  | SUGGEST i ->
    BITSTRING { 13 : 8; Int32.of_int i : 32 }
  | HAVE_ALL ->
    BITSTRING { 14 : 8 }
  | HAVE_NONE ->
    BITSTRING { 15 : 8 }
  | REJECT (i, off, len) ->
    BITSTRING { 16 : 8; Int32.of_int i : 32; Int32.of_int off : 32; Int32.of_int len : 32 }
  | ALLOWED pieces ->
    let rec loop = function
      | [] -> Bitstring.empty_bitstring
      | p :: pieces -> BITSTRING { Int32.of_int p : 32; loop pieces : -1 : bitstring }
    in
    BITSTRING { 17 : 8; loop pieces : -1 : bitstring }
  | EXTENDED (id, s) ->
    BITSTRING { 20 : 8; id : 8; s : -1 : string }

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let write sock msg =
  let doit () =
    let bs = put' msg in
    let len = Bitstring.bitstring_length bs in
    assert (len land 7 = 0);
    let len = BITSTRING { Int32.of_int (len lsr 3) : 32 } in
    (* Bitstring.hexdump_bitstring stderr bs; *)
    Tcp.write_bitstring sock len >>= fun () ->
    Tcp.write_bitstring sock bs
  in
  Lwt.catch doit Lwt.fail

exception BadMsg of int * int

let get len s =
  bitmatch Bitstring.bitstring_of_string s with
  | { _ } as bs when Bitstring.bitstring_length bs = 0 ->
    KEEP_ALIVE
  | { 0 : 8 } ->
    CHOKE
  | { 1 : 8 } ->
    UNCHOKE
  | { 2 : 8 } ->
    INTERESTED
  | { 3 : 8 } ->
    NOT_INTERESTED
  | { 4 : 8; i : 32 : bind (Int32.to_int i) } ->
    HAVE i
  | { 5 : 8; s : -1 : string, bind (Bits.of_bin s) } ->
    BITFIELD s
  | { 6 : 8; i : 32 : bind (Int32.to_int i);
      off : 32 : bind (Int32.to_int off);
      len : 32 : bind (Int32.to_int len) } ->
    REQUEST (i, off, len)
  | { 7 : 8; i : 32 : bind (Int32.to_int i);
      off : 32 : bind (Int32.to_int off);
      s : -1 : string } ->
    PIECE (i, off, s)
  | { 8 : 8; i : 32 : bind (Int32.to_int i);
      off : 32 : bind (Int32.to_int off);
      len : 32 : bind (Int32.to_int len) } ->
    CANCEL (i, off, len)
  | { 9 : 8; port : 16 } ->
    PORT port
  | { 13 : 8; i : 32 : bind (Int32.to_int i) } ->
    SUGGEST i
  | { 14 : 8 } ->
    HAVE_ALL
  | { 15 : 8 } ->
    HAVE_NONE
  | { 16 : 8; i : 32 : bind (Int32.to_int i);
      off : 32 : bind (Int32.to_int off);
      len : 32 : bind (Int32.to_int len) } ->
    REJECT (i, off, len)
  | { 17 : 8; pieces : -1 : bitstring } ->
    let rec loop bs =
      bitmatch bs with
      | { p : 32; bs : -1 : bitstring } -> Int32.to_int p :: loop bs
      | { } -> []
    in
    ALLOWED (loop pieces)
  | { 20 : 8; id : 8; s : -1 : string } ->
    EXTENDED (id, s)
  | { _ } ->
    failwith "can't parse msg"
    (* fail (\* fail (BadMsg (len, id)) *\) *)

let read sock =
  let doit () =
    Tcp.read_int32_be sock >|= Int32.to_int >>= fun len ->
    Tcp.read sock len >|= get len
    (* Bitstring.hexdump_bitstring stderr s; *)
  in
  Lwt.catch doit Lwt.fail

let lt_extension_bit = 43 (* 20-th bit from the right *)
