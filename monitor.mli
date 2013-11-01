type policy =
  | AllForOne
  | OneForOne

type id
type t

module ThreadId : sig
  type t = id
  val compare : t -> t -> int
  val to_string : t -> string
end

val debug : id -> ?exn:exn -> ('a, unit, string, unit Lwt.t) format4 -> 'a

(** [current id] returns the monitor that is supervising the execution
of [id] (if any). *)
val current : id -> t option

(** [create p] creates a new supervisor with the desired policy *)
val create : ?parent:t -> policy -> string -> t

(** [name s] returns the name of the supervisor [s] *)
val name : t -> string

(** [please_die s] shuts down [s]. All processes supervised
by [s] will be canceled *)
val die : t -> unit

(** [kill id] cancels the Lwt thread corresponding to [id] *)
val kill : id -> unit

(** [spawn s f] spawns a new thread supervised by [s] *)
val spawn : ?parent:t -> name:string -> ?cleanup:(unit -> unit Lwt.t) ->
  (id -> unit Lwt.t) -> unit
