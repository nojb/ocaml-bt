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

(** Torrent. Basically keeps track of which blocks/pieces we have and various
    other derived pieces of information. *)

open Event

type t

val create : Metadata.t -> (event -> unit) -> t Lwt.t
(** Create a torrent object corresponding to the given metadata dictionary.
    This loads any partially downloaded data from disk and re-checks all the hashes
    to be sure which pieces are valid. *)

val get_block : t -> int -> int -> int -> Cstruct.t Lwt.t
(** Called to read a block from the already downloaded data with the intention
    of sending it to another peer who has requested it. *)

val is_complete : t -> bool
(** Whether we have all the pieces. *)

val got_block : t -> int -> int -> Cstruct.t -> unit
(** Called when we receive a block from a peer. *)

val amount_left : t -> int64
(** Number of bytes left to download. *)

val numgot : t -> int
(** Number of pieces we have. *)

val have : t -> Bits.t
(** Bitfield of pieces we have. *)

val has_piece : t -> int -> bool
(** Whether we have a particular piece. *)

val has_block : t -> int -> int -> int -> bool
(** Whether we have a particular block. *)

val missing_blocks_in_piece : t -> int -> int
(** How many blocks are we missing in a piece. *)

val have_size : t -> int64
(** How many bytes we have downloaded and verified. *)
