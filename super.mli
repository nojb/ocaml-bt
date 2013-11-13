type restart_policy =
  | AllForOne
  | OneForOne

val default_stop :
  Msg.super_msg Lwt_pipe.t ->
  (Proc.Id.t -> unit Lwt.t)

val start :
  restart_policy ->
  string ->
  children:Msg.child list ->
  super_ch:Msg.super_msg Lwt_pipe.t ->
  ch:Msg.super_msg Lwt_pipe.t ->
  (Proc.Id.t * Msg.super_msg Lwt_pipe.t)
