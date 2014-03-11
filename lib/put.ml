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

type t =
  int * (string -> int -> unit)

let length (n, _) =
  n

let bind (n, f) (m, g) =
  (n+m, fun s i -> f s i; g s (i+n))
  
let (>>=) = bind
let (>>) = bind
  
module type S = sig
  val int64 : int64 -> t
  val int32 : int32 -> t
  val int16 : int -> t
  val int : int -> t
end

external swap16 : int -> int     = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external unsafe_set_8  : string -> int -> int -> unit   = "%string_unsafe_set"
external unsafe_set_16 : string -> int -> int -> unit   = "%caml_string_set16u"
external unsafe_set_32 : string -> int -> int32 -> unit = "%caml_string_set32u"
external unsafe_set_64 : string -> int -> int64 -> unit = "%caml_string_set64u"

module BE = struct
  let int16 n =
    2, fun s i -> if Sys.big_endian then unsafe_set_16 s i n else unsafe_set_16 s i (swap16 n)
  let int32 n =
    4, fun s i -> if Sys.big_endian then unsafe_set_32 s i n else unsafe_set_32 s i (swap32 n)
  let int64 n =
    8, fun s i -> if Sys.big_endian then unsafe_set_64 s i n else unsafe_set_64 s i (swap64 n)
  let int n =
    int32 (Int32.of_int n)
end

module LE = struct
  let int16 n =
    2, fun s i -> if Sys.big_endian then unsafe_set_16 s i (swap16 n) else unsafe_set_16 s i n
  let int32 n =
    4, fun s i -> if Sys.big_endian then unsafe_set_32 s i (swap32 n) else unsafe_set_32 s i n
  let int64 n =
    8, fun s i -> if Sys.big_endian then unsafe_set_64 s i (swap64 n) else unsafe_set_64 s i n
  let int n =
    int32 (Int32.of_int n)
end

let int8 n =
  1, fun s i -> unsafe_set_8 s i n

let string str =
  String.length str, fun s i -> String.blit str 0 s i (String.length str)

let substring s off len =
  if len < 0 || off < 0 || String.length s > off + len then invalid_arg "Put.substring";
  len, fun str i -> String.blit s 0 str i len

let char c =
  1, fun s i -> s.[i] <- c

let run (n, f) =
  let s = String.create n in
  f s 0;
  s
