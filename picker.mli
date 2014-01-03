type t

val create : int -> t
val got_have : t -> int -> unit
val lost_have : t -> int -> unit
val did_request : t -> int -> unit
val got_piece : t -> int -> unit
val next : t -> (int -> bool) -> int option
val is_complete: t -> bool
