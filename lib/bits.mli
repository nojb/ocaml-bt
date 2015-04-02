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

(** bitfields *)

type t

val create : int -> t

val length : t -> int

val resize : t -> int -> unit

val clear : t -> unit
(** [clear b] sets every bit of [b] to [0]. *)

val set_all : t -> unit
(** [set_all b] sets every bit of [b] to [1]. *)

val copy : t -> t
(** Copy the bitfield. *)

val count_ones : t -> int
(** [count_ones b] is the number of [1]'s in [b]. *)

val count_zeroes : t -> int
(** [count_zeroes b size] is the number of those [i], [0 <= i < size] such
    that the [i]-th bit of [b] is [0]. *)

(* val equal : t -> t -> bool *)
(** Whether two bitfields are equal.  They must have the same length to be
    equal. *)

val set : t -> int -> unit
(** [set b i] sets the [i]-th bit of [b] to [1]. *)

val unset : t -> int -> unit
(** [unset b i] sets the [i]-th bit of [b] to [0]. *)

val is_set : t -> int -> bool
(** [unset b i] is [true] if the [i]-th bit of [b] is [1], else [false]. *)

val of_cstruct : Cstruct.t -> t
(** Make a bitfield from the bits of the input string. *)

val to_cstruct : t -> Cstruct.t
(** Pack the bitfield into bytes.  The length of the output string is the
    smallest integer larger or equal to [l/8] where [l] is the length of the
    bitfield. *)

(* val copy_into : t -> t -> unit *)
(** [copy_into b1 b2] copies [b1] into [b2], enlargin [b2] if necessary. *)

val has_all : t -> bool
(** [has_all b size] is [count_zeroes b size = 0]. *)

val blit : t -> int -> t -> int -> int -> unit
