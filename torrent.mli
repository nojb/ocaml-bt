type t

val create : Info.t -> t
val got_have : t -> int -> unit
val lost_have : t -> int -> unit
val request_lost : t -> int -> unit
val request_sent : t -> int -> unit
val got_piece : t -> int -> string -> bool
val completed : t -> Bits.t
val next_piece : t -> (int -> bool) -> (int * int) option
val is_complete : t -> bool
val completion : t -> float
val get_block : t -> int -> int -> int -> string option Lwt.t
