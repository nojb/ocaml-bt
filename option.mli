type 'a t = 'a option

val bind : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t
val fail : 'a -> 'b t
val either : ('a -> 'b t) -> ('a -> 'b t) -> ('a -> 'b t)
val map : ('a -> 'b t) -> 'a list -> 'b list t
val fold : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
