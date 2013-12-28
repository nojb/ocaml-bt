type t

val create : Info.t -> t
val start_external_connection : t -> Lwt_unix.file_descr * Lwt_unix.sockaddr -> unit
val start_connection : t -> Lwt_unix.sockaddr -> unit
