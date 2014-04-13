type 'a t

val create : ?capacity:int -> unit -> 'a t
val push : 'a t -> 'a -> unit Lwt.t
val push' : 'a t -> 'a -> unit
val filter : 'a t -> ('a -> bool) -> unit
val length : 'a t -> int
val is_empty : 'a t -> bool
val assoc : ?equal:('a -> 'a -> bool) -> ('a * 'b) t -> 'a -> 'b
val remove_assoc : ?equal:('a -> 'a -> bool) -> ('a * 'b) t -> 'a -> 'b
val iter : 'a t -> ('a -> unit) -> unit
