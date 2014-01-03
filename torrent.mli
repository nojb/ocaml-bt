type t

val create : Info.t -> t Lwt.t
val request_lost : t -> int -> int -> int -> unit
val got_block : t -> int -> int -> string -> bool
val new_request : t -> int -> (int * int) option
val available_requests : t -> int -> bool
val is_complete : t -> bool
val get_block : t -> int -> int -> int -> string option Lwt.t
