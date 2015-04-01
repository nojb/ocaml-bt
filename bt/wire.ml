(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

type message =
  | KEEP_ALIVE
  | CHOKE
  | UNCHOKE
  | INTERESTED
  | NOT_INTERESTED
  | HAVE of int
  | BITFIELD of Bits.t
  | REQUEST of int * int * int
  | PIECE of int * int * Cstruct.t
  | CANCEL of int * int * int
  | PORT of int
  | HAVE_ALL
  | HAVE_NONE
  | SUGGEST of int
  | REJECT of int * int * int
  | ALLOWED of int list
  | EXTENDED of int * Cstruct.t

let strl f l =
  "[" ^ String.concat " " (List.map f l) ^ "]"

let string_of_message x =
  let open Printf in
  match x with
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

let writer x =
  let open Util.W in
  match x with
  | KEEP_ALIVE ->
      empty
  | CHOKE ->
      byte 0
  | UNCHOKE ->
      byte 1
  | INTERESTED ->
      byte 2
  | NOT_INTERESTED ->
      byte 3
  | HAVE i ->
      byte 4 <+> int i
  | BITFIELD bits ->
      byte 5 <+> string (Bits.to_bin bits)
  | REQUEST (i, off, len) ->
      byte 6 <+> int i <+> int off <+> int len
  | PIECE (i, off, s) ->
      byte 7 <+> int i <+> int off <+> immediate s
  | CANCEL (i, off, len) ->
      byte 8 <+> int i <+> int off <+> int len
  | PORT i ->
      byte 9 <+> int16 i
  | SUGGEST i ->
      byte 13 <+> int i
  | HAVE_ALL ->
      byte 14
  | HAVE_NONE ->
      byte 15
  | REJECT (i, off, len) ->
      byte 16 <+> int i <+> int off <+> int len
  | ALLOWED pieces ->
      byte 17 <+> concat (List.map int pieces)
  | EXTENDED (id, s) ->
      byte 20 <+> byte id <+> immediate s

let writer x =
  let open Util.W in
  let w = writer x in
  int (len w) <+> w

let parse cs =
  let int cs o = Int32.to_int @@ Cstruct.BE.get_uint32 cs o in
  match Cstruct.get_uint8 cs 0 with
  | 0 ->
      CHOKE
  | 1 ->
      UNCHOKE
  | 2 ->
      INTERESTED
  | 3 ->
      NOT_INTERESTED
  | 4 ->
      HAVE (int cs 1)
  | 5 ->
      BITFIELD (Bits.of_bin @@ Cstruct.to_string @@ Cstruct.shift cs 1)
  | 6 ->
      REQUEST (int cs 1, int cs 5, int cs 9)
  | 7 ->
      PIECE (int cs 1, int cs 5, Cstruct.shift cs 9)
  | 8 ->
      CANCEL (int cs 1, int cs 5, int cs 9)
  | 9 ->
      PORT (Cstruct.BE.get_uint16 cs 1)
  | 13 ->
      SUGGEST (int cs 1)
  | 14 ->
      HAVE_ALL
  | 15 ->
      HAVE_NONE
  | 16 ->
      REJECT (int cs 1, int cs 5, int cs 9)
  | 17->
      let rec loop cs =
        if Cstruct.len cs >= 4 then
          let p, cs = Cstruct.split cs 4 in
          int p 0 :: loop cs
        else
          []
      in
      ALLOWED (loop @@ Cstruct.shift cs 1)
  | 20 ->
      EXTENDED (Cstruct.get_uint8 cs 1, Cstruct.shift cs 2)
  | _ ->
      failwith "can't parse msg"

let parse cs =
  if Cstruct.len cs = 0 then
    KEEP_ALIVE
  else
    parse cs

let max_packet_len = 32 * 1024

let ltep_bit = 43 (* 20-th bit from the right *)
let dht_bit = 63 (* last bit of the extension bitfield *)

module Cs = Nocrypto.Uncommon.Cs

module R = struct

  type state = Cstruct.t

  let empty = Cs.empty

  let (<+>) = Cs.(<+>)

  let handle state buf =
    let rec loop cs =
      if Cstruct.len cs > 4 then
        let l = Int32.to_int @@ Cstruct.BE.get_uint32 cs 0 in
        if l + 4 >= Cstruct.len cs then
          let packet, cs = Cstruct.split (Cstruct.shift cs 4) l in
          let cs, packets = loop cs in
          let packet = parse packet in
          cs, packet :: packets
        else
          cs, []
      else
        cs, []
    in
    loop (state <+> buf)

end
