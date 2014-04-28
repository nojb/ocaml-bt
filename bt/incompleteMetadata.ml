type t = {
  ih : SHA1.t;
  length : int;
  raw : string;
  pieces : [ `Done | `Pending | `Missing ] array;
  mutable missing : int
}

let kilobytes n = n * 1024

let info_piece_size = kilobytes 16 (* bytes *)

let roundup n r =
  (n + r - 1) / r * r

let create ih length =
  let npieces = roundup length info_piece_size / info_piece_size in
  { ih; length; raw = String.create length; pieces = Array.create npieces `Missing;
    missing = npieces }

(* let partial_length m = *)
(*   m.length *)

let not_done p n =
  match p.pieces.(n) with
  | `Done -> false
  | `Pending
  | `Missing -> true

let add_piece p n s =
  if not_done p n then begin
    String.blit s 0 p.raw (n * info_piece_size) (String.length s);
    p.pieces.(n) <- `Done;
    p.missing <- p.missing - 1
  end;
  p.missing = 0

let pick_missing p =
  let rec loop pending i =
    if i >= Array.length p.pieces then
      if List.length pending = 0 then None
      else Some (List.nth pending (Random.int (List.length pending)))
    else match p.pieces.(i) with
      | `Missing -> p.pieces.(i) <- `Pending; Some i
      | `Done -> loop pending (i+1)
      | `Pending -> loop (i :: pending) (i+1)
  in
  loop [] 0
      
let verify p =
  if SHA1.digest_of_string p.raw = p.ih then
    Some (String.copy p.raw)
    (* Some (create (Bcode.decode p.raw)) *)
  else
    None

let is_complete p =
  p.missing = 0

let info_hash { ih } = ih
  (* | Complete m -> m.info_hash *)
  (* | Incomplete p -> p.ih *)

let length { length } = length
