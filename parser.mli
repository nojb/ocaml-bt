type 'a p

exception Parsing_error of int

val run_parser : ?full:bool -> string -> ?start:int -> 'a p -> 'a
val return : 'a -> 'a p
val bind : 'a p -> ('a -> 'b p) -> 'b p
val fail : 'a p
val alt : 'a p -> 'a p -> 'a p
val (>>=) : 'a p -> ('a -> 'b p) -> 'b p
val (>>) : 'a p -> 'b p -> 'b p
val (>|=) : 'a p -> ('a -> 'b) -> 'b p
val (>|) : 'a p -> 'b -> 'b p
val (<|>) : 'a p -> 'a p -> 'a p
val sat : ('a -> bool) -> 'a p -> 'a p
val any_char : char p
val char : char -> char p
val chars1_pred : (char -> bool) -> string p
val digits : string p
val many : 'a p -> 'a list p
val many1 : 'a p -> 'a list p
val any_int : int p
val any_int64 : int64 p
val wrapped : 'a p -> 'b p -> 'c p -> 'b p
val string_of_length : int -> string p
val pair : 'a p -> 'b p -> ('a * 'b) p
val fix : (unit -> 'a p) -> 'a p
