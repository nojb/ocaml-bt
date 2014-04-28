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

(* type event = *)
(*   | PieceVerified of int *)
(*   | Loaded of int * int * Bits.t *)
(*   | Completed *)
                            
type t

val create : Metadata.t -> t
val update : t -> (int * int * Bits.t) Lwt.t
val get_block : t -> int -> int -> int -> string Lwt.t
val request_block : t -> (int -> bool) -> (int * int * int) option
val lost_request : t -> int * int * int -> unit
val is_complete : t -> bool
val got_block : t -> int -> int -> string -> [ `Verified | `Failed | `Continue ] Lwt.t
val down : t -> int64
val up : t -> int64
val amount_left : t -> int64
val numgot : t -> int
val have : t -> Bits.t
val has_piece : t -> int -> bool
val got_have : t -> int -> bool
val got_bitfield : t -> Bits.t -> bool
val lost_have : t -> int -> unit
val lost_bitfield : t -> Bits.t -> unit
