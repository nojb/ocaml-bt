type t

type event =
  [ `CONNECTED of Lwt_unix.file_descr * (Word160.t * Bits.t)
  | `FAILED of Unix.sockaddr * exn option ]

val create : id:Word160.t -> info_hash:Word160.t -> t
val connect : t -> Unix.sockaddr -> unit
val accept : t -> Lwt_unix.file_descr * Unix.sockaddr -> unit
val on_event : t -> (event -> unit) -> unit
