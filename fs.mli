val open_and_check_file :
  Proc.Id.t ->
  Torrent.info ->
  ((Lwt_io.input_channel * Lwt_io.output_channel * int64) list * Bits.t) Lwt.t

val start :
  super_ch:Msg.super_msg Lwt_pipe.t ->
  handles:(Lwt_io.input_channel * Lwt_io.output_channel * int64) list ->
  pieces:Torrent.piece_info array ->
  ch:Msg.fs_msg Lwt_pipe.t ->
  Proc.Id.t
