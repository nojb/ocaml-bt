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

(** IO.  Handles reading/writing/connecting to network sockets.  It also allows
    to use a stream cipher to encrypt outgoing data and decrypt incoming data. *)

type t

val close : t -> unit Lwt.t
(** Close the socket. *)
    
val read : t -> string -> int -> int -> int Lwt.t
(** [read sock str pos len] reads at most [len] bytes into [str], starting at
    offset [pos] from [sock].  Returns the number of bytes actually read. *)
    
val write : t -> string -> int -> int -> int Lwt.t
(** [write sock str pos len] writes at most [len] bytes from [str], starting at
    offset [pos] into [sock].  Returns the number of bytes actually written. *)
    
val really_read : t -> string -> int -> int -> unit Lwt.t
(** [really_read sock str pos len] reads exactly [len] bytes into [str],
    starting at offset [pos] from [sock].  Raises [End_of_file] if not anough
    bytes are available. *)
    
val read_char : t -> char Lwt.t
(** [read_char sock] reads a character from [sock]. *)
    
val read_string : t -> int -> string Lwt.t
(** [read_string sock n] reads a string of length exactly [n] from [sock].
    Raises [End_of_file] if not enough bytes are available. *)
    
val read_int16 : t -> int Lwt.t
(** [read_int16 sock] reads a 16-bit big-endian int from [sock]. *)
    
val read_int32 : t -> int32 Lwt.t
(** [read_int32 sock] reads a 32-bit big-endian int from [sock]. *)
    
val really_write : t -> string -> int -> int -> unit Lwt.t
(** [really_write sock str pos len] writes exactly [len] bytes from [str],
    starting at offset [pos] into [sock]. *)
    
val write_string : t -> string -> unit Lwt.t
(** [write_string sock s] writes the string [s] into [sock]. *)
    
val write_bitstring : t -> Bitstring.bitstring -> unit Lwt.t
(** [write_bitstring sock bs] writes the bitstring [bs] into [sock]. *)
    
val write_int16 : t -> int -> unit Lwt.t
(** [write_int16 sock n] writes a 16-bit big-endian int into [sock]. *)
    
val write_int32 : t -> int32 -> unit Lwt.t
(** [write_int32 sock n] writes a 32-bit big-endian int into [sock]. *)
    
val on_write : t -> unit Lwt.t
(** [on_write t] waits until something is written through the socket. *)

val enable_encryption : t -> Cryptokit.Stream.stream_cipher -> unit
(** [enable_encryption sock c] enables encryption of outgoing data using the
    stream cipher [c]. *)
  
val disable_encryption : t -> unit
(** [disable_encryption sock] disables encryption of outgoing data. *)
  
val enable_decryption : t -> Cryptokit.Stream.stream_cipher -> unit
(** [enable_decryption sock c] enables decryption of incoming data using the
    stream cipher [c]. *)
  
val disable_decryption : t -> unit
(** [disable_decryption sock] disables decryption of incoming data. *)

val is_encrypted : t -> bool
(** Whether the socket is either encrypting the output or decrypting the
    input. *)

val create : Addr.t -> t
(** Creates a socket to connect to a given address. *)

val of_file_descr : Lwt_unix.file_descr -> t
(** Creates a socket from a connected TCP socket. *)
  
val connect : t -> unit Lwt.t
(** Connect the socket. *)
    
val reconnect : t -> unit Lwt.t
(** Close the socket if necessary and then connect again. *)

val addr : t -> Addr.t
(** The socket address. *)

val out_channel : ?buffer_size:int -> t -> Lwt_io.output_channel
(** Create a buffered output channel that writes to the given socket. *)
                                             
val in_channel : ?buffer_size:int -> t -> Lwt_io.input_channel
(** Create a buffered input channel that reads from the given socket. *)
