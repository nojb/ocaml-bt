module type PRIO = sig
  type t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val compare : t -> t -> int
end

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type prio
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : key -> t -> bool
  val singleton : key -> t
  val add : key -> t -> t
  val find : key -> t -> prio
  val min_elt : t -> key
  val remove_min : t -> t
  val remove : key -> t -> t
  val remove_all : key -> t -> t
  val of_list : key list -> t
  val to_list : t -> (key * prio) list
  val pick : (key -> bool) -> t -> key list
end

module Make (O : ORD) (P : PRIO) : S with type key = O.t and type prio = P.t

module Int : PRIO
