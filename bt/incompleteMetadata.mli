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

(** Incomplete Metadata.  Used to keep track of the data we are receiving when
    acquiring the metainfo dictionary using ut_metadata. *)

type t

val create : SHA1.t -> int -> t
(** [create ih len] creates an incomplete metadata object which info-hash [ih]
    and length [len]. *)
  
val add_piece : t -> int -> string -> bool
(** Called when a metainfo piece has been received.  Returns [true] if the all
    the pieces have been received (even though they may contain corrupted
    data). *)
  
val get_next_metadata_request : t -> int option
(** Which metainfo piece should be requested next.  Returns [None] if no
    suitable piece can be found. *)
    
val verify : t -> string option
(** Verify whether the SHA1 hash of the incomplete data matches the know
    info-hash.  Returns [Some s] if it does, where [s] is the raw bencoded data.
    Otherwise returns [None]. *)
    
val is_complete : t -> bool
(** Whether all the pieces have been received. *)
  
val info_hash : t -> SHA1.t
(** The info-hash of the metainfo dictionary. *)
                       
val length : t -> int
(** The length of the bencoded metainfo dictionary. *)
