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

type socket

val create_socket : unit -> socket
val close : socket -> unit Lwt.t
val write : socket -> string -> unit Lwt.t
val write_bitstring : socket -> Bitstring.bitstring -> unit Lwt.t
val read : socket -> int -> string Lwt.t
val read_int32_be : socket -> int32 Lwt.t
val write_int32_be : socket -> int32 -> unit Lwt.t
val connect : socket -> Addr.t -> unit Lwt.t
val listen : ?backlog:int -> socket -> int -> (socket -> Addr.t -> unit) -> (unit -> unit)
val getpeeraddr : socket -> Addr.t
val io : socket -> Lwt_io.input_channel * Lwt_io.output_channel
