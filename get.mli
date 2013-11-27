type 'a t

module type S = sig
  val uint8 : int t
  val sint8 : int t
  val uint16 : int t
  val sint16 : int t
  val int32 : int32 t
  val int64 : int64 t
end

module BE : S
module LE : S

val string : string t
val end_of_input : unit t
val either : 'a t -> 'a t -> 'a t
    
val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
val return : 'a -> 'a t
val fail : 'a t
    
exception Get_error

val run : 'a t -> string -> 'a
val run_full : 'a t -> string -> 'a
