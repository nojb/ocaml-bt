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

type t = {
  mutable bytes : int;
  mutable reset : float;
  mutable last : float
}

let now () =
  Unix.gettimeofday ()

let create () =
  let now = now () in
  {bytes = 0; reset = now; last = now}

let add r count =
  r.bytes <- r.bytes + count;
  if r.reset = 0.0 then r.reset <- now ();
  r.last <- now ()

let get r =
  if r.last = r.reset then 0.0
  else (float r.bytes) /. (r.last -. r.reset)

let reset r =
  r.bytes <- 0;
  let now = now () in
  r.reset <- now;
  r.last <- now

let compare r1 r2 =
  if get r1 > get r2 then 1
  else -1

let pp fmt r =
  Format.fprintf fmt "%s/s" (Util.string_of_file_size (Int64.of_float (get r)))
