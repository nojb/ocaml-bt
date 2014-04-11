type socket

val create_socket : unit -> socket
val close : socket -> unit Lwt.t
val write : socket -> string -> unit Lwt.t
val read : socket -> int -> string Lwt.t
val connect : socket -> Addr.t -> unit Lwt.t
val listen : ?backlog:int -> socket -> int -> (socket -> Addr.t -> unit) -> (unit -> unit)
val getpeeraddr : socket -> Addr.t
(* val set_timeout : socket -> float -> unit *)
(* val bind : socket -> Addr.t -> unit *)
(* val listen : ?backlog:int -> socket -> unit *)
(* val accept : socket -> (socket * Addr.t) Lwt.t *)
