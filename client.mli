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

val create : Info.t -> t Lwt.t
(* val add_handler : t -> (update -> unit) -> unit *)
