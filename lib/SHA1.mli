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

(** SHA1 hashes: blobs of 20-bytes.  They are supposed to be immutable, but see
    {!to_raw}, {!of_raw}. *)

type t
(** The type of SHA1 hashes. *)

val zero : t
(** The hash [0x00000000000000000000]. *)

val last : t
(** The hash [0xFFFFFFFFFFFFFFFFFFFF]. *)

val compare : t -> t -> int
(** Compare the hashes bitwise. *)

val equal : t -> t -> bool
(** Whether two hashes are identical. *)

val to_raw : t -> Cstruct.t
(** The underlying data.  WARNING: this is not a copy, so it is possibly to
    modify in-place.  *)

val of_raw : Cstruct.t -> t
(** WARNING: does not make a copy. *)

val digest : Cstruct.t -> t
(** Compute the SHA1 digest of a string. *)

val digestv : Cstruct.t list -> t
(** [digestv l] is [digest (Cs.concat l)], but more efficient. *)

val to_z : t -> Nocrypto.Numeric.Z.t
(** The big int represented by the hash's bits using big-endian ordering.  *)

val of_z : Nocrypto.Numeric.Z.t -> t
(** Create a hash from the first 20 bytes from a big int.  The sign is ignored
    and it uses a big-endian ordering. *)

val distance : t -> t -> Nocrypto.Numeric.Z.t
(** The XOR-distance between two hashes.  Used in {!Kademlia}. *)

val generate : ?g:Nocrypto.Fortuna.g -> ?prefix:string -> unit -> t
(** A random hash with prefix [prefix]. *)

val of_hex : string -> t
(** The hash with the given hexadecimal characters. *)

val to_hex : t -> string
(** Convert the hash to 40 hexadecimal characters. *)

val to_hex_short : t -> string
(** The first 7 hexadecimal characters of the hash. *)

val of_base32 : string -> t
(** The hash with the given base32 characters. *)
