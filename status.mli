val start :
  super_ch : Msg.super_msg Lwt_pipe.t ->
  ch : Msg.status_msg Lwt_pipe.t ->
  Proc.Id.t
