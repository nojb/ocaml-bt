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

type t

type addr = Unix.inet_addr * int

val keepalive_delay : float

exception Timeout

open Event

type event_callback = event -> unit

val create_has_meta : SHA1.t -> Metadata.t -> t

val create_no_meta : SHA1.t -> t

val id : t -> SHA1.t
(** The peer ID. *)

val extended_handshake : t -> Wire.message
(** Send LTEP handshake.  Currently supported extension are [ut_metadata] and
    [ut_pex]. *)

val peer_choking : t -> bool
(** Whether the peer is choking us. *)

val peer_interested : t -> bool
(** Whether the peer is interested in us. *)

val am_choking : t -> bool
(** Whether we are choking this peer. *)

val am_interested : t -> bool
(** Whether we are interested in this peer. *)

val has_piece : t -> int -> bool
(** Whether this peer has a particular piece. *)

val has : t -> Bits.t
(** The bitfield of pieces this peer has. *)

val request_meta_piece : t -> int -> Wire.message

val choke : t -> Wire.message
(** Send a CHOKE message.  If the peer is already choked, do not send
    anything. *)

val unchoke : t -> Wire.message
(** Send an UNCHOKE message.  If the peer is already unchoked, do not send
    anything. *)

val interested : t -> Wire.message
(** Send a INTERESTED command.  If we are already interested in this peer, do
    not send anything. *)

val not_interested : t -> Wire.message
(** Send a NOT_INTERESTED message.  If we are already not interested in this
    peer, do not send anything. *)

val have : t -> int -> Wire.message
(** Send a HAVE message.  If this peer already has the piece, do not send
    anything. *)

val have_bitfield : t -> Bits.t -> Wire.message
(** Send a BITFIELD message. *)

val send_cancel : t -> int * int -> Wire.message
(** Send a CANCEL message. *)

val send_port : t -> int -> Wire.message
(** Send a PORT message (used for DHT). *)

val send_reject_meta : t -> int -> Wire.message
(** Reject a request for a metainfo piece. *)

val send_meta_piece : t -> int -> int * Cstruct.t -> Wire.message
(** Send a metainfo piece. *)

val send_block : t -> int -> int -> string -> Wire.message
(** Send a block. *)

val upload_rate : t -> float
(** The current upload speed. *)

val download_rate : t -> float
(** The current download speed. *)

val reset_rates : t -> unit
(** Reset the download/upload speed computation. *)

val worked_on_piece : t -> int -> bool
(** Whether this peer has sent us blocks of a particular piece. *)

val strike : t -> int
(** Mark this peer as having participated in a piece that failed its SHA1 hash
    check.  Returns the updated number of strikes. *)

val is_seed : t -> bool
(** Whether this peer already has all the pieces. *)

val time : t -> float
(** Peer creation time. *)

val piece_data_time : t -> float
(** The last time this peer sent us a block. *)

val close : t -> unit
(** Disconnect this peer. *)

val got_message : t -> Wire.message -> event

val send_pex : t -> addr list -> unit
(** Sends a periodic PEX message (if supported).  The address list passed
    is the list of currently connected peers. *)

val is_snubbing : t -> bool
(** Whether the peer has sent any block in the last 30 seconds. *)

val to_string : t -> string
(** Print out the peer's id (in shortened form) and address. *)

val download_speed : t -> float

val upload_speed : t -> float
