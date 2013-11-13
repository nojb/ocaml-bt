type restart_policy =
  | AllForOne
  | OneForOne

val default_stop :
  (Msg.super_msg option -> unit) ->
  (Proc.Id.t -> unit Lwt.t)

val start :
  restart_policy ->
  string ->
  children:Msg.child list ->
  send_super:(Msg.super_msg option -> unit) ->
  msgs:Msg.super_msg Lwt_stream.t ->
  send:(Msg.super_msg option -> unit) ->
  (Proc.Id.t * (Msg.super_msg option -> unit))
