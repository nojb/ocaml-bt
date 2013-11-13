type 'a t

val create : unit -> 'a t
val write : 'a t -> 'a -> unit
val read : 'a t -> 'a Lwt.t
val close : 'a t -> unit
val iter : ('a -> unit) -> 'a t -> unit Lwt.t
val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
