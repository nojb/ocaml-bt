module Ip : sig
  type t

  val any : t
  val loopback : t
  val of_string : string -> t
  val of_string_noblock : string -> t Lwt.t
  val to_string : t -> string
  val of_ints : int -> int -> int -> int -> t
  val to_ints : t -> int * int * int * int
end

type t = Ip.t * int

val port : t -> int
  
val ip : t -> Ip.t

val to_string : t -> string
  
val to_string_compact : t -> string

val to_sockaddr : t -> Unix.sockaddr

val of_sockaddr : Unix.sockaddr -> t
