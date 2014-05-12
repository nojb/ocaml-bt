module type KEY = sig
  type t
  val length : int
  val nth : t -> int -> bool
  val equal : t -> t -> bool
end

module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : ?max:int -> ?pred:('a -> bool) -> key -> 'a t -> (key * 'a) list
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

include S with type key = SHA1.t
