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

val close : t -> unit Lwt.t
val read : t -> string -> int -> int -> int Lwt.t
val write : t -> string -> int -> int -> int Lwt.t
val really_read : t -> string -> int -> int -> unit Lwt.t
val read_char : t -> char Lwt.t
val read_string : t -> int -> string Lwt.t
val read_int16 : t -> int Lwt.t
val read_int32 : t -> int32 Lwt.t
val really_write : t -> string -> int -> int -> unit Lwt.t
val write_string : t -> string -> unit Lwt.t
val write_bitstring : t -> Bitstring.bitstring -> unit Lwt.t
val write_int16 : t -> int -> unit Lwt.t
val write_int32 : t -> int32 -> unit Lwt.t
val on_write : t -> unit Lwt.t
val enable_encryption : t -> Cryptokit.Stream.stream_cipher -> unit
val disable_encryption : t -> unit
val enable_decryption : t -> Cryptokit.Stream.stream_cipher -> unit
val disable_decryption : t -> unit

val create : Addr.t -> t

val connect : t -> unit Lwt.t
val reconnect : t -> unit Lwt.t

val addr : t -> Addr.t

val out_channel : ?buffer_size:int -> t -> Lwt_io.output_channel
val in_channel : ?buffer_size:int -> t -> Lwt_io.input_channel
