type address
(* type t *)

val listen_on : int -> (string * address) Lwt_stream.t * (unit -> unit)
val to_string : address -> string
val to_string_compact : address -> string
