let _ = Random.self_init ()
    
type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t = {
  name : string;
  info_hash : Word160.t;
  hashes : Word160.t array;
  piece_length : int;
  total_length : int64;
  files : file_info list;
  encoded : string
}

let length info =
  String.length info.encoded

let kilobytes n = n * 1024

let info_piece_size = kilobytes 16 (* bytes *)
                      
let roundup n r =
  (n + r - 1) / r * r

let get_piece info i =
  let l = String.length info.encoded in
  let numpieces =
    roundup l info_piece_size / info_piece_size
  in
  if i >= numpieces || i < 0 then invalid_arg "Info.get_piece";
  if i < numpieces - 1 then
    String.sub info.encoded (i * info_piece_size) info_piece_size
  else
    let last_piece_size = l mod info_piece_size in
    String.sub info.encoded (i * info_piece_size) last_piece_size

(* let comment bc = *)
(*   try Some (Bcode.find "comment" bc |> Bcode.to_string) *)
(*   with Not_found -> None *)

(* let creation_date bc = *)
(*   Bcode.search_string "creation date" bc *)

let either f g x =
  try f x with _ -> g x

(* let announce_list bc = *)
(*   let announce_list () = *)
(*     Bcode.find "announce-list" bc |> Bcode.to_list |> *)
(*     List.map (fun l -> *)
(*         Bcode.to_list l |> List.map (fun s -> Bcode.to_string s |> Uri.of_string)) *)
(*   in *)
(*   let announce () = *)
(*     let announce = Bcode.find "announce" bc |> Bcode.to_string in *)
(*     [[Uri.of_string announce]] *)
(*   in *)
                      (** announce_list takes precedence over announce - see BEP 12 *)
(*   either announce_list announce () *)

let split_at n s =
  let l = String.length s in
  if l mod n <> 0 then invalid_arg "Torrent.split_at";
  Array.init (l/n) (fun i -> String.sub s (n*i) n)

let hashes bc =
  Bcode.find "pieces" bc |> Bcode.to_string |>
  split_at 20 |> Array.map Word160.from_bin

let info_hash (bc : Bcode.t) =
  Bcode.bencode bc |> Put.run |> Word160.digest_of_string

let piece_length bc =
  Bcode.find "piece length" bc |> Bcode.to_int

let name bc =
  Bcode.find "name" bc |> Bcode.to_string

let files bc =
  let name = Bcode.find "name" bc |> Bcode.to_string in
  let many_files () =
    Bcode.find "files" bc |> Bcode.to_list |>
    List.map (fun d ->
        let file_size = Bcode.find "length" d |> Bcode.to_int64 in
        let path = Bcode.find "path" d |> Bcode.to_list |> List.map Bcode.to_string in
        {file_size; file_path = name :: path})
  in
  let single_file () =
    let n = Bcode.find "length" bc |> Bcode.to_int64 in
    [{file_size = n; file_path = [name]}]
  in
  either many_files single_file ()

let total_length (bc : Bcode.t) =
  let many_files () =
    Bcode.find "files" bc |> Bcode.to_list |>
    List.fold_left (fun acc d -> Bcode.find "length" d |> Bcode.to_int64 |> Int64.add acc) 0L
  in
  let single_file () =
    Bcode.find "length" bc |> Bcode.to_int64
  in
  either single_file many_files ()

open Format

let rec pp_announce_list fmt xs =
  let rec loop fmt = function
    | [] -> ()
    | [x] -> fprintf fmt "[@[<hov 2>%a@]]" pp_tier x
    | x :: xs -> fprintf fmt "[@[<hov 2>%a@]]@,%a" pp_tier x loop xs
  in loop fmt xs

and pp_tier fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%s" (Uri.to_string x)
  | x :: xs -> fprintf fmt "%s;@ %a" (Uri.to_string x) pp_tier xs

let pp_files fmt files =
  let rec loop i fmt = function
  | [] -> ()
  | fi :: files ->
      fprintf fmt "%d. %s (%s)@,%a" i (String.concat "/" fi.file_path)
        (Util.string_of_file_size fi.file_size) (loop (i+1)) files
  in loop 1 fmt files

let create bc =
  let name = name bc in
  let hashes = hashes bc in
  let info_hash = info_hash bc in
  let piece_length = piece_length bc in
  let total_length = total_length bc in
  let files = files bc in
  { name; info_hash; piece_length; total_length;
    hashes; files; encoded = Put.run (Bcode.bencode bc) }

let piece_length info i =
  assert (i >= 0 && i < Array.length info.hashes);
  if i < Array.length info.hashes - 1 then info.piece_length
  else Int64.(sub info.total_length (mul (of_int i) (of_int info.piece_length))) |>
       Util.safe_int64_to_int

let piece_offset info i =
  assert (i >= 0 && i < Array.length info.hashes);
  Int64.(mul (of_int i) (of_int info.piece_length))

let pp fmt info =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "             name: %s@," info.name;
  Format.fprintf fmt "        info hash: %s@," (Word160.to_hex info.info_hash);
  (* Format.fprintf fmt "    announce-list: @[<v>%a@]@," pp_announce_list announce_list; *)
  Format.fprintf fmt "     total length: %s@," (Util.string_of_file_size info.total_length);
  Format.fprintf fmt "     piece length: %s@," (Util.string_of_file_size (Int64.of_int info.piece_length));
  Format.fprintf fmt " number of pieces: %d@," (Array.length info.hashes);
  Format.fprintf fmt "            files: @[<v>%a@]" pp_files info.files;
  Format.fprintf fmt "@]@."

let absolute_offset meta idx off =
  Int64.add (piece_offset meta idx) (Int64.of_int off)

type partial = {
  ih : Word160.t;
  length : int;
  raw : string;
  pieces : [ `Done | `Pending | `Missing ] array;
  mutable missing : int
}

let create_partial ih length =
  let npieces = roundup length info_piece_size / info_piece_size in
  { ih; length; raw = String.create length; pieces = Array.create npieces `Missing;
    missing = npieces }

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
  if Word160.digest_of_string p.raw = p.ih then
    Some (create (Get.run Bcode.bdecode p.raw))
  else
    None

let is_complete p =
  p.missing = 0
