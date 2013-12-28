type t

val create : int -> t
val length : t -> int
val copy : t -> t
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
val iteri : (int -> bool -> unit) -> t -> unit
val map : (bool -> bool) -> t -> t
val fold_left_i : ('a -> int -> bool -> 'a) -> 'a -> t -> 'a
val to_string : t -> string
val of_bin : string -> t
val to_bin : t -> string
val blit : t -> int -> t -> int -> int -> unit
val to_array : t -> bool array
val of_array : bool array -> t
val to_list : t -> int list
val of_list : int list -> t
