val open_and_check_file :
  (* Monitor.id -> *)
  Supervisor.thread_id ->
  Torrent.info ->
  ((Lwt_io.input_channel * Lwt_io.output_channel * int64) list * Bits.t) Lwt.t
val start :
  msg_supervisor:(Supervisor.supervisor_msg -> unit) ->
  (* monitor:Monitor.t -> *)
  handles:(Lwt_io.input_channel * Lwt_io.output_channel * int64) list ->
  pieces:Torrent.piece_info array ->
  fs:Messages.fs_msg Lwt_stream.t ->
  unit
