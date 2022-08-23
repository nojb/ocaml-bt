type t =
  | Int of int
  | String of string
  | List of t list
  | Dict of (string * t) list

val to_sexp : t Sexp.Encoder.t

val decode : string -> t option

val encode : t -> string

module Decoder : sig
  type bencode

  type 'a t

  val int : int t

  val string : string t

  val list : 'a t -> 'a list t

  val member : string -> 'a t -> 'a t

  val if_member : string -> 'a t -> 'a t -> 'a t

  val value : bencode t

  val query : 'a t -> bencode -> 'a option

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end
with type bencode := t
