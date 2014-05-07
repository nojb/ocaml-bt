type t

val create_no_meta : SHA1.t -> SHA1.t -> (Peer.t -> Peer.event_callback) ->
  (Peer.t -> Peer.get_metadata_func) -> t
    
val create_has_meta : SHA1.t -> SHA1.t -> (Peer.t -> Peer.event_callback) ->
  Metadata.t -> Torrent.t -> (Peer.t -> Peer.get_block_func) -> t

val start : t -> unit
val fold_peers : (Peer.t -> 'a -> 'a) -> t -> 'a -> 'a
val iter_peers : (Peer.t -> unit) -> t -> unit
val peer_finished : t -> Peer.t -> unit
val handle_incoming_peer : t -> IO.socket -> Addr.t -> unit
val handle_received_peer : t -> Addr.t -> unit
val torrent_loaded : t -> Metadata.t -> Torrent.t -> (Peer.t -> Peer.get_block_func) -> unit
val got_bad_piece : t -> int -> unit
val got_piece : t -> int -> unit
(** Sends a HAVE message to all connected peers. *)
