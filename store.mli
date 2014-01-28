type t

val create : Info.file_info list -> t Lwt.t
val close : t -> unit
val read : t -> int64 -> int -> string Lwt.t
val write : t -> int64 -> string -> unit Lwt.t
