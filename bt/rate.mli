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

(** Rates.  Used to keep track of upload/download speed. *)

type t

val create : unit -> t
(** Create a new speed rate. *)
  
val add : t -> int -> unit
(** [add r n] adds [n] transferred bytes to the rate [r]. *)
  
val get : t -> float
(** [get r] returns the current rate of transfer in [r]. *)
  
val reset : t -> unit
(** [reset r] resets the transfer rate in [r].  This should be called
    periodically so that the rate calculation stays accurate. *)
  
val compare : t -> t -> int
(** Compare two rates. *)
  
val pp : Format.formatter -> t -> unit
(** Pretty print a rate. *)
