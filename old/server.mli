type t

val create : (Lwt_unix.file_descr * Unix.sockaddr -> unit) -> t
val stop : t -> unit
val port : t -> int
