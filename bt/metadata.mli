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

(** Metadata.  It handles the piece/block <-> piece/ofs/len abstraction. *)

type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t

val create : Bcode.t -> t
(** Parse a bencoded metainfo dictionary *)
  
val get_piece : t -> int -> string
(** Get a metainfo piece to send to a peer that has requested it via
    ut_metadata. *)
  
val length : t -> int
(** Total length of the metainfo in bencoded form. *)
  
val block_number : t -> int -> int -> int
(** [block_number m i ofs] returns the block number corresponding to that
    piece/offs combination.  [i] is not necessary but is required for consistency. *)
  
val total_length : t -> int64
(** Total length of the torrent. *)
  
val piece_count : t -> int
(** Number of pieces. *)
  
val piece_length : t -> int -> int
(** Length of a particular piece.  All the pieces are the same length except,
    possibly, the last one. *)
  
val piece_offset : t -> int -> int64
(** The global offset at which the given piece begins.  Used as an argument to
    {!Store.read} and {!Store.write}. *)
  
val block_count : t -> int -> int
(** How many blocks does the given piece have. *)
  
val pp : Format.formatter -> t -> unit
(** Pretty prints the metainfo. *)
  
val block_offset : t -> int -> int -> int64
(** Return the global offset at which the piece/block begins.  Used as an
    argument to {!Store.read} and {!Store.write}. *)
  
(* val block_size : t -> int -> int -> int *)
  
val hash : t -> int -> SHA1.t
(** The SHA1 hash of the given piece. *)
                         
val block : t -> int -> int -> int * int * int
(** The piece/ofs/len corresponding to the given piece/block. *)
                               
val iter_files : t -> (file_info -> unit Lwt.t) -> unit Lwt.t
(** Iterates over all the files listen in the metainfo dictionary. *)
