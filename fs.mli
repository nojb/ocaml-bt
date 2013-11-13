val open_and_check_file :
  Torrent.info ->
  Proc.Id.t ->
  (* Torrent.info -> *)
  ((Lwt_io.input_channel * Lwt_io.output_channel * int64) list * Bits.t) Lwt.t

val start :
  send_super:(Msg.super_msg option -> unit) ->
  handles:(Lwt_io.input_channel * Lwt_io.output_channel * int64) list ->
  pieces:Torrent.piece_info array ->
  (Proc.Id.t * (Msg.fs_msg option -> unit))
