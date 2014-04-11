type socket

val create_socket : unit -> socket
  
val send : socket -> string -> Addr.t -> unit Lwt.t
val recv : socket -> (string * Addr.t) Lwt.t
