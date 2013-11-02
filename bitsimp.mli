type t

val zero : int -> t
val length : t -> int
val count : t -> int
val equal : t -> t -> bool
val set : t -> int -> unit
val unset : t -> int -> unit
val toggle : t -> int -> unit
val is_set : t -> int -> bool
val lognot : t -> t
val logand : t -> t -> t
val logor : t -> t -> t
val iter : (bool -> unit) -> t -> unit
val map : (bool -> bool) -> t -> t
val to_string : t -> string
val of_bin : string -> t
val to_bin : t -> string
val pad : int -> t -> t
