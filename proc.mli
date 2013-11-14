module Id : sig
  type t
  val to_string : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

val debug : Id.t -> ?exn:exn -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val kill : Id.t -> unit
(* val run : ?name:string -> (Id.t -> 'a Lwt.t) -> 'a Lwt.t *)
val async : ?name:string -> (Id.t -> unit Lwt.t) -> unit
val spawn : ?name:string -> (Id.t -> unit Lwt.t) -> (Id.t -> unit Lwt.t) -> (Id.t -> unit Lwt.t) -> Id.t
