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

type t

val create_no_meta : SHA1.t -> SHA1.t -> (Peer.t -> Peer.event_callback) ->
  (Peer.t -> Peer.get_metadata_func) -> t
    
val create_has_meta : SHA1.t -> SHA1.t -> (Peer.t -> Peer.event_callback) ->
  Metadata.t -> Torrent.t -> (Peer.t -> Peer.get_block_func) -> t

val start : t -> unit
val fold_peers : (Peer.t -> 'a -> 'a) -> t -> 'a -> 'a
val iter_peers : (Peer.t -> unit) -> t -> unit
val peer_finished : t -> Peer.t -> unit
val handle_incoming_peer : t -> IO.t -> Addr.t -> unit
val handle_received_peer : t -> Addr.t -> unit
val torrent_loaded : t -> Metadata.t -> Torrent.t -> (Peer.t -> Peer.get_block_func) -> unit
val got_bad_piece : t -> int -> unit
val got_piece : t -> int -> unit
(** Sends a HAVE message to all connected peers. *)
val upload_speed : t -> float
val download_speed : t -> float
val num_connected_peers : t -> int
val num_total_peers : t -> int
