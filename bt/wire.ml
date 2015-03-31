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
    BITSTRING { 7 : 8; Int32.of_int i : 32; Int32.of_int off : 32; Cstruct.to_string s : -1 : string }
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
    BITSTRING { 20 : 8; id : 8; Cstruct.to_string s : -1 : string }

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
(* fail (\* fail (BadMsg (len, id)) *\) *)

let parse cs =
  if Cstruct.len cs = 0 then
    KEEP_ALIVE
  else
    parse cs

let max_packet_len = 32 * 1024

(* let read input = *)
(*   let doit () = *)
(*     Lwt_io.BE.read_int input >>= fun len -> *)
(*     assert (len <= max_packet_len); *)
(*     if len = 0 then *)
(*       Lwt.return KEEP_ALIVE *)
(*     else *)
(*       let buf = String.create len in *)
(*       Lwt_io.read_into_exactly input buf 0 len >|= fun () -> parse buf *)
(*       (\* Bitstring.hexdump_bitstring stderr s; *\) *)
(*   in *)
(*   Lwt.catch doit Lwt.fail *)

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
