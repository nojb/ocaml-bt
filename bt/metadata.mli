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

type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t
(*   name : string; *)
(*   info_hash : SHA1.t; *)
(*   hashes : SHA1.t array; *)
(*   piece_count : int; *)
(*   piece_length : int; *)
(*   block_size : int; *)
(*   last_piece_size : int; *)
(*   total_length : int64; *)
(*   files : file_info list; *)
(*   encoded : string *)
(* } *)

val create : Bcode.t -> t
val get_piece : t -> int -> string
val length : t -> int
val block_number : t -> int -> int
val total_length : t -> int64
val piece_count : t -> int
val piece_length : t -> int -> int
val piece_offset : t -> int -> int64
val block_count : t -> int -> int
val pp : Format.formatter -> t -> unit
val block_offset : t -> int -> int -> int64
(* val block_size : t -> int -> int -> int *)
val hash : t -> int -> SHA1.t
val block : t -> int -> int -> int * int * int
val iter_files : t -> (file_info -> unit Lwt.t) -> unit Lwt.t
