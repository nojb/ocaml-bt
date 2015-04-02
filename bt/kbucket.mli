module type KEY = sig
  type t
  val nth : t -> int -> bool
  val length : t -> int
  val equal : t -> t -> bool
  val distance : t -> t -> Z.t
  val pp : Format.formatter -> t -> unit
end

module type DATA = sig
  type t
  val equal : t -> t -> bool
  val merge : t -> t -> t
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type key
  type data
  type t
  val empty : ?k:int -> ?p:int -> key -> t
  val length : t -> int
  val add : key -> data -> t -> [ `Ok of t | `Ping of (key * data) list ]
  val get : key -> t -> data option
  val remove : key -> t -> t
  val closest : key -> int -> t -> (key * data) list
  val to_list : t -> (key * data) list
  val pp : Format.formatter -> t -> unit
end

module Make (K : KEY) (D : DATA) : S with type key = K.t and type data = D.t
