type t

val create_with_partial :
  Lwt_unix.sockaddr ->
  Word160.t ->
  Lwt_io.input_channel ->
  Lwt_io.output_channel ->
  (* Info.partial -> *)
  Word160.t ->
  (Info.t -> unit) ->
  Bits.t ->
  t

val partial_exts : Bits.t
  
val stop : t -> unit
val is_choked : t -> bool
val is_interested : t -> bool
(* val interesting : t -> unit *)
(* val not_interesting : t -> unit *)
val to_string : t -> string
val choke : t -> unit
val unchoke : t -> unit
val piece_completed : t -> int -> unit
val ul : t -> float
val dl : t -> float
