type t

val create : Word160.t -> t
val start_external_connection : t -> Lwt_unix.file_descr * Lwt_unix.sockaddr -> unit
val start_connection : t -> Lwt_unix.sockaddr -> unit
val port : t -> int
val id : t -> Word160.t
