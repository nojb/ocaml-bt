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

(** SHA1 hashes *)

(** The type of hashes.  The string should be 20-bytes long. *)
type t = private string

val zero : t
(** The hash [0x00000000000000000000]. *)
  
val last : t
(** The hash [0xFFFFFFFFFFFFFFFFFFFF]. *)
  
val compare : t -> t -> int
(** Compare the hashes bitwise. *)
  
val equal : t -> t -> bool
(** Whether two hashes are identical. *)
  
val hash : t -> int
(** A suitable function for {!Hashtbl.Make}. *)
  
val to_hex : t -> string
(** Convert the hash to 40 hexadecimal characters. *)
  
val to_hex_short : t -> string
(** The first 7 hexadecimal characters of the hash. *)
  
val pp : Format.formatter -> t -> unit
(** Print the hexadecimal characters of the hash. *)
  
val to_bin : t -> string
(** The underlying 20-byte string. *)
  
val of_bin : string -> t
(** The hash corresponding to the given 20-byte string. *)
  
val string : string -> t
(** Compute the SHA1 digest of a string. *)
  
val to_z : t -> Z.t
(** The (large) integer represented by the hash's bits using big-endian
    ordering.  *)
    
val of_z : Z.t -> t
(** Create a hash from the first 20 bytes from a large integer.  The sign is
    ignored and it uses a big-endian ordering. *)
  
val distance : t -> t -> Z.t
(** The XOR-distance between two hashes.  Used in {!Kademlia}. *)
    
val random : unit -> t
(** A random hash. *)
  
val peer_id : string -> t
(** Generate a BitTorrent peer ID according to usual conventions.  The argument
    should be a distinctive string of 3-4 characters. *)
  
val of_hex : string -> t
(** The hash with the given hexadecimal characters. *)
  
val of_base32 : string -> t
(** The hash with the given base32 characters. *)
  
val strings : string list -> t
(** The SHA1 digest of the string obtained by concatenating the given list of
    strings. *)
