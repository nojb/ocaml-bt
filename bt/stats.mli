type t = {
  upload_speed : float;
  download_speed : float;
  num_connected_peers : int;
  num_total_peers : int;
  downloaded : int64;
  total_size : int64;
  have_pieces : int;
  total_pieces : int;
  amount_left : int64
}
