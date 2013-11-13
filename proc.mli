module Id : sig
  type t
  val to_string : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

val kill : Id.t -> unit
val spawn : (Id.t -> unit Lwt.t) -> Id.t
val cleanup : (Id.t -> unit Lwt.t) -> (Id.t -> unit Lwt.t) -> (Id.t -> unit
Lwt.t) -> (Id.t -> unit Lwt.t)
val catch : (Id.t -> unit Lwt.t) -> (Id.t -> unit Lwt.t) -> (Id.t -> unit Lwt.t)

