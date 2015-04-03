(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

(** Metadata.  It handles the piece/block <-> piece/ofs/len abstraction. *)

type t

val create : Bcode.t -> t
(** Parse a bencoded metainfo dictionary *)

val get_piece : t -> int -> Cstruct.t
(** Get a metainfo piece to send to a peer that has requested it via
    ut_metadata. *)

val length : t -> int
(** Total length of the metainfo in bencoded form. *)

val total_length : t -> int64
(** Total length of the torrent. *)

val piece_count : t -> int
(** Number of pieces. *)

val block_size : t -> int

val piece_length : t -> int -> int
(** Length of a particular piece.  All the pieces are the same length except,
    possibly, the last one. *)

val offset : t -> int -> int -> int64

val block_count : t -> int -> int
(** How many blocks does the given piece have. *)

val pp : Format.formatter -> t -> unit
(** Pretty prints the metainfo. *)

val hash : t -> int -> SHA1.t
(** The SHA1 hash of the given piece. *)

val files : t -> (string list * int64) list
