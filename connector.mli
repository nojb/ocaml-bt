type t

val create : Word160.t -> Word160.t -> t
val connect : t -> Unix.sockaddr -> unit
val accept : t -> Lwt_unix.file_descr * Unix.sockaddr -> unit
val got_peer : t -> Unix.sockaddr -> unit
