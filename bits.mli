type t

val zero : int -> t
val length : t -> int
val equal : t -> t -> bool
val set : int -> t -> t
val unset : int -> t -> t
val toggle : int -> t -> t
val is_set : int -> t -> bool
val lognot : t -> t
val logand : t -> t -> t
val logor : t -> t -> t
val iter : (bool -> unit) -> t -> unit
val map : (bool -> bool) -> t -> t
val to_string : t -> string
val of_bin : string -> t
val to_bin : t -> string
val pad : int -> t -> t
