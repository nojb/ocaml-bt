type metadata_node = {
  mutable requested_at : float;
  piece : int
}

type t = {
  ih : SHA1.t;
  length : int;
  raw : string;
  pieces_needed : metadata_node option array;
  mutable missing : int
}

let kilobytes n = n * 1024

let info_piece_size = kilobytes 16 (* bytes *)

let roundup n r =
  (n + r - 1) / r * r

let create ih length =
  let npieces = roundup length info_piece_size / info_piece_size in
  let pieces_needed = Array.init npieces (fun i -> Some {requested_at = 0.0; piece = i}) in
  { ih; length; raw = String.create length; pieces_needed; missing = npieces }

let add_piece p n s =
  let rec loop i =
    match p.pieces_needed.(i) with
    | Some { piece; _ } when piece = n ->
      String.blit s 0 p.raw (n * info_piece_size) (String.length s);
      p.pieces_needed.(i) <- None;
      p.missing <- p.missing - 1
    | _ ->
      loop (i+1)
  in
  loop 0;
  p.missing = 0

let min_repeat_interval_secs = 3

let get_next_metadata_request p =
  let now = Unix.time () in
  let rec loop i =
    if i >= Array.length p.pieces_needed then None
    else match p.pieces_needed.(i) with
      | None -> loop (i+1)
      | Some pc ->
        if pc.requested_at +. float min_repeat_interval_secs < now then begin
          pc.requested_at <- now;
          Array.blit p.pieces_needed 1 p.pieces_needed 0 (Array.length p.pieces_needed - 1);
          p.pieces_needed.(Array.length p.pieces_needed - 1) <- Some pc;
          Some pc.piece
        end
        else None
  in
  loop 0
      
let verify p =
  if SHA1.digest_of_string p.raw = p.ih then
    Some (String.copy p.raw)
  else
    None

let is_complete p =
  let rec loop i =
    if i >= Array.length p.pieces_needed then true
    else match p.pieces_needed.(i) with
      | None -> loop (i+1)
      | Some _ -> false
  in
  loop 0

let info_hash { ih } = ih

let length { length } = length
