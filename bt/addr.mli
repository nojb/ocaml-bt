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

module Ip : sig
  type t

  val any : t
  val loopback : t
  val of_string : string -> t
  val of_string_noblock : string -> t Lwt.t
  val to_string : t -> string
  val of_ints : int -> int -> int -> int -> t
  val to_ints : t -> int * int * int * int
  val of_string_compact : Bitstring.bitstring -> t
end

type t = Ip.t * int

val port : t -> int
  
val ip : t -> Ip.t

val to_string : t -> string
  
val to_string_compact : t -> string

val to_sockaddr : t -> Unix.sockaddr

val of_sockaddr : Unix.sockaddr -> t

val of_string_compact : Bitstring.bitstring -> t

module Set : Set.S with type elt = t
