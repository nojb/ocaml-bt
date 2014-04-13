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

type t

val create : Meta.t -> t
val update : t -> unit Lwt.t
val get_block : t -> int -> int -> int -> string Lwt.t
(* val got_piece : t -> int -> string -> bool *)
val peer_ready : t -> Peer.t -> unit
val did_request : t -> int -> unit
(* val request_lost : t -> int -> unit *)
val is_complete : t -> bool
val got_block : t -> int -> int -> int -> bool
(* val get_block : t -> int -> int -> int -> string Lwt.t *)
val down : t -> int64
val up : t -> int64
val amount_left : t -> int64
val numgot : t -> int
val have : t -> Bits.t
val has_piece : t -> int -> bool
val next : t -> Bits.t -> int option
val got_have : t -> int -> unit
val got_bitfield : t -> Bits.t -> unit
val lost_have : t -> int -> unit
val lost_bitfield : t -> Bits.t -> unit
