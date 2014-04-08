module type KEY = sig
  type t
  val length : int
  val nth : t -> int -> bool
  val equal : t -> t -> bool
end

module Make (Key : KEY) : sig
  type 'a t

  val empty : 'a t
  val add : Key.t -> 'a -> 'a t -> 'a t
  val find : ?max:int -> ?pred:('a -> bool) -> Key.t -> 'a t -> (Key.t * 'a) list
  val iter : (Key.t -> 'a -> unit) -> 'a t -> unit
end
