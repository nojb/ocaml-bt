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

let strl f l =
  "[" ^ String.concat " " (List.map f l) ^ "]"

let string_of_message = function
  | KEEP_ALIVE -> "keep alive"
  | CHOKE -> "choke"
  | UNCHOKE -> "unchoke"
  | INTERESTED -> "interested"
  | NOT_INTERESTED -> "not interested"
  | HAVE i -> sprintf "have %d" i
  | BITFIELD b -> sprintf "bitfield with %d/%d pieces" (Bits.count b) (Bits.length b)
  | REQUEST (i, off, len) -> sprintf "request %d off:%d len:%d" i off len
  | PIECE (i, off, _) -> sprintf "piece %d off:%d" i off
  | CANCEL (i, off, len) -> sprintf "cancel %d off:%d len:%d" i off len
  | PORT port -> sprintf "port %d" port
  | HAVE_ALL -> "have all"
  | HAVE_NONE -> "have none"
  | SUGGEST i -> sprintf "suggest %d" i
  | REJECT (i, off, len) -> sprintf "reject %d off:%d len:%d" i off len
  | ALLOWED pieces -> sprintf "allowed %s" (strl string_of_int pieces)
  | EXTENDED (id, _) -> sprintf "extended %d" id

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

let write output msg =
  let doit () =
    let (bs, boff, blen) = put' msg in
    assert (boff land 7 = 0 && blen land 7 = 0);
    (* Bitstring.hexdump_bitstring stderr bs; *)
    Lwt_io.BE.write_int output (blen lsr 3) >>= fun () ->
    Lwt_io.write_from_exactly output bs (boff lsr 3) (blen lsr 3)
    (* Tcp.write_bitstring sock len >>= fun () -> *)
    (* Tcp.write_bitstring sock bs *)
  in
  Lwt.catch doit Lwt.fail

let parse s =
  bitmatch Bitstring.bitstring_of_string s with
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

let max_packet_len = 32 * 1024

let read input =
  let doit () =
    Lwt_io.BE.read_int input >>= fun len ->
    assert (len <= max_packet_len);
    if len = 0 then
      Lwt.return KEEP_ALIVE
    else
      let buf = String.create len in
      Lwt_io.read_into_exactly input buf 0 len >|= fun () -> parse buf
      (* Bitstring.hexdump_bitstring stderr s; *)
  in
  Lwt.catch doit Lwt.fail

let ltep_bit = 43 (* 20-th bit from the right *)
let dht_bit = 63 (* last bit of the extension bitfield *)
