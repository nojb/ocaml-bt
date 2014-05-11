type t = {
  upload_speed : float;
  (** Speed at which we are uploading data to the connected peers. *)
  download_speed : float;
  (** Speed at which we are downloading data from the connected peers. *)
  num_connected_peers : int;
  (** Number of connected peers. *)
  num_total_peers : int;
  (** Number of known peers. *)
  downloaded : int64;
  (** Amount of bytes downloaded (or [0L] if not known). *)
  total_size : int64;
  (** Total size of the torrent (or [0L] if not known). *)
  have_pieces : int;
  (** Number of pieces we have (or [0] if not known). *)
  total_pieces : int;
  (** Total number of pieces (or [0] if not known). *)
  amount_left : int64
  (** Bytes left to download (or [0L] if not known). *)
}
