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

module type PRIO = sig
  type t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val compare : t -> t -> int
end

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type prio
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : key -> t -> bool
  val singleton : key -> t
  val add : key -> t -> t
  val find : key -> t -> prio
  val min_elt : t -> key
  val remove_min : t -> t
  val remove : key -> t -> t
  val remove_all : key -> t -> t
  val of_list : key list -> t
  val to_list : t -> (key * prio) list
  val pick : (key -> bool) -> t -> key list
end

module Make (O : ORD) (P : PRIO) : S with type key = O.t and type prio = P.t

(* module Int : PRIO with type t = int *)

include S with type key = int and type prio = int
