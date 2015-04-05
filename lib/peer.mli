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

(** Peer *)

module ARC4 = Nocrypto.Cipher_stream.ARC4

type addr = Unix.inet_addr * int

type pex_flags =
  { pex_encryption : bool;
    pex_seed : bool;
    pex_utp : bool;
    pex_holepunch : bool;
    pex_outgoing : bool }

type event =
  | Choked of (int * int * int) list
  | Unchoked
  | Interested
  | NotInterested
  | Have of int
  | HaveBitfield of Bits.t
  | BlockRequested of int * int * int
  | BlockReceived of int * int * Cstruct.t
  | PeerDisconnected of (int * int * int) list
  | AvailableMetadata of int
  | MetaRequested of int
  | GotMetaPiece of int * Cstruct.t
  | RejectMetaPiece of int
  | GotPEX of (addr * pex_flags) list * addr list
  | DHTPort of int

type t

val create : SHA1.t -> (event -> unit) -> Lwt_unix.file_descr -> (ARC4.key * ARC4.key) option -> t

val id : t -> SHA1.t
(** The peer ID. *)

val peer_choking : t -> bool
(** Whether the peer is choking us. *)

val peer_interested : t -> bool
(** Whether the peer is interested in us. *)

val am_choking : t -> bool
(** Whether we are choking this peer. *)

val am_interested : t -> bool
(** Whether we are interested in this peer. *)

val has : t -> int -> bool
(** Whether this peer has a particular piece. *)

val bitfield : t -> Bits.t
(** The bitfield of pieces this peer has. *)

val worked_on_piece : t -> int -> bool
(** Whether this peer has sent us blocks of a particular piece. *)

val strike : t -> int
(** Mark this peer as having participated in a piece that failed its SHA1 hash
    check.  Returns the updated number of strikes. *)

val to_string : t -> string
(** Print out the peer's id (in shortened form) and address. *)

val download_speed : t -> float

val upload_speed : t -> float

val requests : t -> int

val requested : t -> int -> int -> int -> bool

(** Outgoing *)

val request_metadata_piece : t -> int -> unit

val choke : t -> unit
(** Send a CHOKE message.  If the peer is already choked, do not send
    anything. *)

val unchoke : t -> unit
(** Send an UNCHOKE message.  If the peer is already unchoked, do not send
    anything. *)

val interested : t -> unit
(** Send a INTERESTED command.  If we are already interested in this peer, do
    not send anything. *)

val not_interested : t -> unit
(** Send a NOT_INTERESTED message.  If we are already not interested in this
    peer, do not send anything. *)

val have : t -> int -> unit
(** Send a HAVE message.  If this peer already has the piece, do not send
    anything. *)

val have_bitfield : t -> Bits.t -> unit
(** Send a BITFIELD message. *)

val cancel : t -> int -> int -> int -> unit
(** Send a CANCEL message. *)

val send_port : t -> int -> unit
(** Send a PORT message (used for DHT). *)

val reject_metadata_request : t -> int -> unit
(** Reject a request for a metainfo piece. *)

val metadata_piece : int -> int -> Cstruct.t -> t -> unit
(** Send a metainfo piece. *)

val request : t -> int -> int -> int -> unit

val piece : t -> int -> int -> Cstruct.t -> unit
(** Send a block. *)

val send_pex : addr list -> t -> unit
(** Sends a periodic PEX message (if supported).  The address list passed
    is the list of currently connected peers. *)
