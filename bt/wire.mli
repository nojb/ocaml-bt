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

val string_of_message : message -> string
(* val read : Lwt_io.input_channel -> message Lwt.t *)
val write : Lwt_io.output_channel -> message -> unit Lwt.t

val ltep_bit : int
val dht_bit : int

module R : sig
  type state
  val empty : state
  val handle : state -> Cstruct.t -> state * message list
end
