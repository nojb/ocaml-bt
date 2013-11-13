val start :
  super_ch:Msg.super_msg Lwt_pipe.t ->
  Lwt_io.input_channel ->
  peer_ch:Msg.msg_ty Lwt_pipe.t ->
  Proc.Id.t
