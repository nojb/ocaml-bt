type t

val create : Metadata.t -> Torrent.t -> t
val get_next_requests : t -> Peer.t -> int -> (int * int) list
val peer_declined_all_requests : t -> Peer.t -> unit
val got_block : t -> Peer.t -> int -> int -> unit
val got_piece : t -> int -> unit
val got_bad_piece : t -> int -> unit
val got_have : t -> int -> unit
val got_bitfield : t -> Bits.t -> unit
val lost_have : t -> int -> unit
val lost_bitfield : t -> Bits.t -> unit
