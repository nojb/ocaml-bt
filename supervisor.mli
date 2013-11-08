module Id : sig
  type t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

type thread_id = Id.t
type supervisor_msg

type restart_policy =
  | AllForOne
  (** If any of the supervisor's children dies, then
      all the rest are canceled and the supervisor's death
      is signaled to its supervisor in turn *)
  | OneForOne
  (** If any of the supervisor's children dies, it
      is noted but no action is taken in response to it *)

val debug :
  thread_id ->
  ?exn:exn ->
  ('a, unit, string, unit Lwt.t) format4 ->
  'a

val spawn:
  ?msg_supervisor:(supervisor_msg -> unit) ->
  name:string ->
  ?cleanup:(unit -> unit Lwt.t) ->
  (thread_id -> unit Lwt.t) ->
  thread_id

val spawn_worker:
  (supervisor_msg -> unit) ->
  string ->
  (thread_id -> unit Lwt.t) ->
  unit

val spawn_supervisor:
  (supervisor_msg -> unit) ->
  string ->
  restart_policy ->
  (supervisor_msg -> unit)

val start :
  restart_policy ->
  supervisor_msg Lwt_stream.t ->
  (thread_id -> unit Lwt.t)

val top_level :
  string ->
  restart_policy ->
  (supervisor_msg -> unit)
