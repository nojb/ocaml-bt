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

val alt : 'a t -> 'a t -> 'a t
val (>>) : 'a t -> 'b t -> 'b t
val (>|) : 'a t -> 'b -> 'b t
val (<|>) : 'a t -> 'a t -> 'a t

val sat : ('a -> bool) -> 'a t -> 'a t
val any_char : char t
val char : char -> char t
val chars1_pred : (char -> bool) -> string t
val digits : string t
val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t
val any_int : int t
val any_int64 : int64 t
val wrapped : 'a t -> 'b t -> 'a t -> 'b t
val string_of_length : int -> string t
val pair : 'a t -> 'b t -> ('a * 'b) t
val fix : (unit -> 'a t) -> 'a t

exception Get_error

(* val run : 'a t -> string -> 'a *)
val run : 'a t -> string -> 'a
val run_file : 'a t -> string -> 'a
