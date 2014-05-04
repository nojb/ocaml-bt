type t

val create : SHA1.t -> int -> t
val add_piece : t -> int -> string -> bool
(* val pick_missing : t -> int option *)
val get_next_metadata_request : t -> int option
val verify : t -> string option
val is_complete : t -> bool
val info_hash : t -> SHA1.t
val length : t -> int
