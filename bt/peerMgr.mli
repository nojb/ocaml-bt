type t

val create : SHA1.t -> SHA1.t -> (Peer.t -> Peer.event -> unit) ->
  (Peer.t -> int -> (int * int * int) list) ->
  (Peer.t -> unit -> int option) -> t
val start : t -> unit
val fold_peers : (Peer.t -> 'a -> 'a) -> t -> 'a -> 'a
val iter_peers : (Peer.t -> unit) -> t -> unit
val peer_finished : t -> Peer.t -> unit
val handle_incoming_peer : t -> IO.socket -> Addr.t -> unit
val handle_received_peer : t -> Addr.t -> unit
