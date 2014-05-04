type metadata_node = {
  mutable requested_at : float;
  piece : int
}
type t = {
  ih : SHA1.t;
  length : int;
  raw : string;
  pieces_needed : metadata_node option array;
  (* pieces : [ `Done | `Pending | `Missing ] array; *)
  mutable missing : int
}

let kilobytes n = n * 1024

let info_piece_size = kilobytes 16 (* bytes *)

let roundup n r =
  (n + r - 1) / r * r

let create ih length =
  let npieces = roundup length info_piece_size / info_piece_size in
  (* let pieces_needed = Lwt_sequence.create () in *)
  let pieces_needed = Array.init npieces (fun i -> Some {requested_at = 0.0; piece = i}) in
  (* for i = 0 to length / info_piece_size do *)
  (*   Queue.push {requested_at = 0.0; piece = i} pieces_needed *)
  (* done; *)
  (* let rec loop i = *)
  (*   if i * info_piece_size >= length then [] *)
  (*   else {requested_at = 0.0; piece = i} :: loop (i+1) *)
  (* in *)
  (* let pieces_needed = loop 0 in *)
  { ih; length; raw = String.create length; pieces_needed; missing = npieces }

(* let has_piece p n = *)
(*   match p.pieces_needed.(n) with *)
(*   | None -> true *)
(*   | Some _ -> false *)
  (* try *)
  (*   Queue.iter (fun pc -> if pc.piece = n then raise Exit) p.pieces_needed; *)
  (*   false *)
  (* with *)
  (* | Exit -> true *)
  (* match p.pieces.(n) with *)
  (* | `Done -> false *)
  (* | `Pending *)
  (* | `Missing -> true *)

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
  (* if not (has_piece p n) then begin (\* not_done p n then begin *\) *)
  (*   String.blit s 0 p.raw (n * info_piece_size) (String.length s); *)
  (*   p.pieces_needed.(n) <- None; *)
  (*   (\* p.pieces.(n) <- `Done; *\) *)
  (*   (\* p.missing <- p.missing - 1 *\) *)
  (* end *)
  (* p.missing = 0 *)

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
  (* if Queue.is_empty p.pieces_needed then None *)
  (* else *)
  (*   let pc = Queue.peek p.pieces_needed in *)
  (*   if pc.requested_at +. float min_repeat_interval_secs < now then begin *)
  (*     pc.requested_at <- now; *)
  (*     Queue.push (Queue.pop p.pieces_needed) p.pieces_needed; *)
  (*     Some pc.piece *)
  (*   end *)
  (*   else None *)

(* let pick_missing p = *)
(*   let rec loop pending i = *)
(*     if i >= Array.length p.pieces then *)
(*       if List.length pending = 0 then None *)
(*       else Some (List.nth pending (Random.int (List.length pending))) *)
(*     else match p.pieces.(i) with *)
(*       | `Missing -> p.pieces.(i) <- `Pending; Some i *)
(*       | `Done -> loop pending (i+1) *)
(*       | `Pending -> loop (i :: pending) (i+1) *)
(*   in *)
(*   loop [] 0 *)
      
let verify p =
  if SHA1.digest_of_string p.raw = p.ih then
    Some (String.copy p.raw)
    (* Some (create (Bcode.decode p.raw)) *)
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
  (* Queue.is_empty p.pieces_needed *)
  (* p.missing = 0 *)

let info_hash { ih } = ih
  (* | Complete m -> m.info_hash *)
  (* | Incomplete p -> p.ih *)

let length { length } = length
