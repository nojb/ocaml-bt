type t

type event =
  | CHOKED of t
  | READY of t
  | PIECE_AVAILABLE of t * int
  | GOT_BITFIELD of t * Bits.t
  | PIECE_SENT of t * int
  | PIECE_COMPLETED of t * int * string
  | DISCONNECTED of t
  | ERROR of t * exn

(* val string_of_event : event -> string *)

val create :
  (* Info.stats -> *)
  Lwt_unix.sockaddr ->
  Word160.t ->
  Lwt_io.input_channel ->
  Lwt_io.output_channel ->
  (* Info.piece_info array -> *)
  Info.t ->
  Store.t ->
  (* fs_ch: Fs.msg Lwt_pipe.t -> *)
  t

val is_choked : t -> bool
val is_interested : t -> bool
val interesting : t -> unit
val not_interesting : t -> unit
val to_string : t -> string
val choke : t -> unit
val unchoke : t -> unit
val available_pieces : t -> Bits.t
val download_piece : t -> int -> unit
val piece_completed : t -> int -> unit
val requested_piece : t -> int option
val add_handler : t -> (event -> unit) -> unit
val ul : t -> float
val dl : t -> float
