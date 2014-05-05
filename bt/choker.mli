type t

val create : PeerMgr.t -> Torrent.t -> t
val start : t -> unit
