val start :
  send_super:(Msg.super_msg option -> unit) ->
  Lwt_io.output_channel ->
  Msg.sender_msg Lwt_stream.t ->
  send_peer:(Msg.msg_ty option -> unit) ->
  Proc.Id.t
