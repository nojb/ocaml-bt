type t

module type S = sig
  val int64 : int64 -> t
  val int32 : int32 -> t
  val int16 : int -> t
  val int8  : int -> t
end

module BE : S
module LE : S

val string : string -> t
val substring : string -> int -> int -> t
val char : char -> t
  
val bind : t -> t -> t
  
val (>>=) : t -> t -> t
val (>>) : t -> t -> t

val run : t -> string
