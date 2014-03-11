type 'a t

val create : ?random:bool -> int -> 'a t
val clear : 'a t -> unit
val reset : 'a t -> unit
val copy : 'a t -> 'a t
val add : 'a t -> 'a -> unit
val mem : 'a t -> 'a -> bool
val remove : 'a t -> 'a -> unit
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val cardinal : 'a t -> int
val choose : 'a t -> 'a
