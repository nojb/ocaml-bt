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

val create : int -> t
val length : t -> int
val copy : t -> t
val count : t -> int
val equal : t -> t -> bool
val set : t -> int -> unit
val unset : t -> int -> unit
val toggle : t -> int -> unit
val is_set : t -> int -> bool
val lognot : t -> t
val logand : t -> t -> t
val logor : t -> t -> t
val logandnot : t -> t -> t
val iter : (bool -> unit) -> t -> unit
val iteri : (int -> bool -> unit) -> t -> unit
val map : (bool -> bool) -> t -> t
val fold_left_i : ('a -> int -> bool -> 'a) -> 'a -> t -> 'a
val to_string : t -> string
val of_bin : string -> t
val to_bin : t -> string
val blit : t -> int -> t -> int -> int -> unit
val to_array : t -> bool array
val of_array : bool array -> t
val to_list : t -> int list
val of_list : int list -> t
