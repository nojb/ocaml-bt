type update = {
  name : string;
  completed : int;
  total : int;
  total_length : int64;
  left : int64;
  available : int;
  requested : int;
  connected : int;
  known : int;
  ul : float;
  dl : float
}

type t

val create : ((Lwt_io.input_channel * Lwt_io.output_channel * int64) list * Bits.t) -> Info.t -> t
val start : t -> unit
val add_handler : t -> (update -> unit) -> unit
