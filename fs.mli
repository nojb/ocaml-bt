type msg =
  [ `CheckPiece of int * bool Lwt_mvar.t
  | `ReadBlock of int * Wire.block * string Lwt_mvar.t
  | `WritePiece of int * string ]

val open_and_check_file :
  Proc.Id.t ->
  Info.t ->
  ((Lwt_io.input_channel * Lwt_io.output_channel * int64) list * Bits.t) Lwt.t

val start :
  handles: (Lwt_io.input_channel * Lwt_io.output_channel * int64) list ->
  pieces: Info.piece_info array ->
  ch: msg Lwt_pipe.t ->
  Proc.Id.t
