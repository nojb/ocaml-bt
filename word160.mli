type t

val zero : t
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val to_hex : t -> string
val to_hex_short : t -> string
val pp : Format.formatter -> t -> unit
val print : out_channel -> t -> unit
val sprint : unit -> t -> string
val to_bin : t -> string
val from_bin : string -> t
val digest_of_string : string -> t
val to_z : t -> Z.t
val dist : t -> t -> Z.t
val random : unit -> t
val peer_id : string -> t
