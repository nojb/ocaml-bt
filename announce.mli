type ann_event =
  | ANN_RESPONSE of int option * int option
  | ANN_PEERS of (Unix.inet_addr * int) list
       
type t

val create : Info.t -> Info.stats -> t
val start : t -> unit
val stop : t -> unit
val add_handler : t -> (ann_event -> unit) -> unit
