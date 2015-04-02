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

(** Requester.  Handles the selection of which blocks to request from which
    peers.  It also tracks the rarity of each piece (i.e., how many peers have
    each piece).  If a request takes too long to fulfill the request is
    cancelled and the corresponding peer is sent a CANCEL message. *)

type t

val create : Metadata.t -> Torrent.t -> t
(** Creates a requester. *)
  
val get_next_requests : t -> Peer.t -> int -> (int * int) list
(** [get_next_requests r p n] returns a list of [(piece, block)] elements
    consisting of at most [n] blocks to request from peer [p].  It favors
    already started pieces over not-yet-started ones, rarer pieces over ones
    that are held more broadly. *)
    
val peer_declined_all_requests : t -> Peer.t -> unit
(** Called when a peer disconnects or chokes us. *)
  
val got_block : t -> Peer.t -> int -> int -> unit
(** Called when the peer send us a block. *)
  
val got_piece : t -> int -> unit
(** Called when a piece is complete and its SHA1 hash is verified. *)
  
val got_bad_piece : t -> int -> unit
(** Called when a piece fails its SHA1 hash check.  Currently it does not do
    anything. *)
  
val got_have : t -> int -> unit
(** Called when a peer sends us a HAVE message.  Used to keep track of the piece
    rarity. *)
  
val got_bitfield : t -> Bits.t -> unit
(** Called when a peer sends us a BITFIELD message.  Used to keep track of piece
    rarity. *)
  
(* val lost_have : t -> int -> unit *)

val lost_bitfield : t -> Bits.t -> unit
(** Called with the piece bitfield of a disconnecting peer.  Used to keep track
    of piece rarity. *)
