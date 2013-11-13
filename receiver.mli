val start :
  send_super:(Msg.super_msg option -> unit) ->
  Lwt_io.input_channel ->
  send_peer:(Msg.msg_ty option -> unit) ->
  Proc.Id.t
