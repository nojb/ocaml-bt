type t

val create : (Lwt_io.input_channel * Lwt_io.output_channel * int64) list -> Info.t -> t
val read_block : t -> int -> Wire.block -> string Lwt.t
val write_piece : t -> int -> string -> unit
val open_and_check_file : Info.t ->
  ((Lwt_io.input_channel * Lwt_io.output_channel * int64) list * Bits.t) Lwt.t
