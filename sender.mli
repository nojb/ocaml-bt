open Messages

val start :
  (* monitor:Monitor.t -> *)
  msg_supervisor:(Supervisor.supervisor_msg -> unit) ->
  Lwt_io.output_channel ->
  sender_msg Lwt_stream.t ->
  (msg_ty -> unit) ->
  unit
