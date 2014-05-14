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

type t = private string

val zero : t
val last : t
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val to_hex : t -> string
val sprint : unit -> t -> string
val to_hex_short : t -> string
val pp : Format.formatter -> t -> unit
val print : out_channel -> t -> unit
val sprint : unit -> t -> string
val to_bin : t -> string
val from_bin : string -> t
val digest_of_string : string -> t
val to_z : t -> Z.t
val of_z : Z.t -> t
val distance : t -> t -> Z.t
val random : unit -> t
val peer_id : string -> t
val of_hex : string -> t
val of_base32 : string -> t
val pp : Format.formatter -> t -> unit
val strings : string list -> t
