type t = Atom of string | List of t list

module Encoder : sig
  type sexp
  type 'a t = 'a -> sexp

  val int : int t
  val string : string t
  val list : 'a t -> 'a list t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val array : 'a t -> 'a array t
  val record : (string * sexp) list -> sexp
  val variant : string -> sexp list -> sexp
  val run : 'a t -> 'a -> sexp
end
with type sexp := t

val print : Format.formatter -> t -> unit
