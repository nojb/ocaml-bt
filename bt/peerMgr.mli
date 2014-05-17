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

(** Peer Manager.  Apart from general manteinance, it takes care of
    disconnecting peers who do not send us piece information for too long
    (currently, 90 seconds). *)

type t

val create_no_meta : SHA1.t -> SHA1.t -> (Peer.t -> Peer.event_callback) ->
  (Peer.t -> Peer.get_metadata_func) -> t
(** [create_no_meta id ih cb r] creates a peer manager without metainfo
    information.  [id] is the client ID, [ih] is the torrent info-hash, [cb] is
    the callback to invoke on important peer events (see {!Peer.event}) and [r]
    is the function used to select missing pieces from the metadata to request
    from other peers. *)

val create_has_meta : SHA1.t -> SHA1.t -> (Peer.t -> Peer.event_callback) ->
  Metadata.t -> Torrent.t -> (Peer.t -> Peer.get_block_func) -> t
(** [create_has_meta id ih cb m t r] creates a peer manager with metainfo
    information.  [id] is the client ID, [ih] is the torrent info-hash, [cb] is the
    callback to invoke on important peer events (see {!Peer.event}), [m] is the
    metainfo dictionary, [t] is the torrent object, and [r] is the function used to
    select missing blocks to request from other peers. *)

val start : t -> unit
(** Start the peer manager event loop. *)

val fold_peers : (Peer.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_peers f pm x] computes [... (f p2 (f p1 x))] where [p1, p2, ...] are
    the connected peers. *)

val iter_peers : (Peer.t -> unit) -> t -> unit
(** [iter_peers f pm] computes [f p1; f p2; ...] where [p1, p2, ...] are the
    connected peers. *)

val peer_finished : t -> Peer.t -> unit
(** Called when a peer disconnects. It removes the peer from the connected
    set. *)

val handle_incoming_peer : t -> IO.t -> unit
(** Called when receiving an incoming peer connection.  It negotiates the
    handshake and starts sharing, or closes the connection and saves the address
    for later if too many peers are already connected. *)

val handle_received_peer : t -> Addr.t -> unit
(** Called when receiving address information for a peer.  It will be saved to
    the pool of `known' peers and a connection may be open in the future if more
    peers are necessary. *)

val torrent_loaded : t -> Metadata.t -> Torrent.t -> (Peer.t -> Peer.get_block_func) -> unit
(** Called when the Torrent has finished loading.  It puts the peer manager in
    the block-requesting stage.  This means that all currently connected peers will
    be put in the block-requesting stage, and all future peers will be created in
    this state.  Also all currently connected peers will be sent HAVE messages to
    let them now which pieces we have. *)

val got_bad_piece : t -> int -> unit
(** Called when the SHA1 hash of a received piece does not match the information
    in the metainfo dictionary.  All connected peers that participated in the
    piece are given a `strike'.  After a sufficient number of strikes
    (currently, 5) a peer is disconnected. *)

val got_piece : t -> int -> unit
(** Called when a piece is received and its SHA1 hash is verified
    successfully.  All connected peers are sent a corresponding HAVE message. *)

val upload_speed : t -> float
(** The speed at which we are uploading to the connected peers. *)

val download_speed : t -> float
(** The speed at which we are receiving data from the connected peers. *)
  
val num_connected_peers : t -> int
(** Number of connected peers. *)
  
val num_total_peers : t -> int
(** Number of known peers. *)
