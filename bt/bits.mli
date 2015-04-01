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

(** Bitfields *)

type t

val create : int -> t
(** Create a bitfield of a given length. *)

val length : t -> int
(** The length of the bitfield. *)

val clear : t -> unit
(** Fill the bitfield with zeroes. *)

val set_all : t -> unit
(** Fill the bitfield with ones. *)

val copy : t -> t
(** Copy the bitfield. *)

val count : t -> int
(** The number of ones in the bitfield. *)

val equal : t -> t -> bool
(** Whether two bitfields are equal.  They must have the same length to be
    equal. *)

val set : t -> int -> unit
(** Set a bit to one. *)

val unset : t -> int -> unit
(** Set a bit to zero. *)

val is_set : t -> int -> bool
(** Whether a bit is set. *)

val of_bin : string -> t
(** Make a bitfield from the bits of the input string. *)

val to_bin : t -> string
(** Pack the bitfield into bytes.  The length of the output string is the
    smallest integer larger or equal to [l/8] where [l] is the length of the
    bitfield. *)

val blit : t -> int -> t -> int -> int -> unit
(** [blit b1 pos1 b2 pos2 len] copies len bits from [b1], starting at position
    [pos1] to [b2], starting at position [pos2].  [b1] and [b2] may be equal. *)

val has_all : t -> bool
(** Whether all the bits in the bitfield are set. *)

val missing : t -> int
(** The number of zeroes in the bitfield. *)
