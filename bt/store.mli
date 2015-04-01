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

(** Store.  Used to read/write blocks/pieces from disk.  Its main function is to
    present an interface as if a collection of files was just one big continuous
    file (this is the point of view of the BitTorrent protocol).  Currently
    there is no effort to perform caching, even though this is probably a good
    idea in the long run for better performance. *)

type t

val create : (string list * int64) list -> t Lwt.t
(** Creates a store to access the files specified in
    the given metainfo dictionary. *)

val close : t -> unit Lwt.t
(** Close all the files in the store. *)

val read : t -> int64 -> int -> Cstruct.t Lwt.t
(** [read st ofs len] reads [len] bytes starting at offset [ofs] from the store
    [st].  The offset and the length can span multiple files in the store. *)

val write : t -> int64 -> Cstruct.t -> unit Lwt.t
(** [write st ofs s] writes [s] starting at offset [ofs] in the store [st].  The
    string may end up being written in more than one file in the store. *)
