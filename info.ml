type piece_info = {
  piece_offset  : int64;
  piece_length  : int;
  piece_digest  : Word160.t
}

type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t = {
  name : string;
  comment : string option;
  info_hash : Word160.t;
  announce_list : Uri.t list list;
  pieces : piece_info array;
  piece_length : int;
  total_length : int64;
  files : file_info list;
}

type stats = {
  mutable uploaded : int64;
  mutable downloaded : int64;
  mutable left : int64;
  mutable local_port : int;
  id : Word160.t;
  completed : Bits.t
  (* info_hash : Word160.t *)
}

let comment bc =
  try Some (Bcode.find "comment" bc |> Bcode.to_string)
  with Not_found -> None

(* let creation_date bc = *)
(*   Bcode.search_string "creation date" bc *)

let either f g x =
  try f x with _ -> g x

let announce_list bc =
  let announce_list () =
    Bcode.find "announce-list" bc |> Bcode.to_list |>
    List.map (fun l ->
        Bcode.to_list l |> List.map (fun s -> Bcode.to_string s |> Uri.of_string))
  in
  let announce () =
    let announce = Bcode.find "announce" bc |> Bcode.to_string in
    [[Uri.of_string announce]]
  in
  either announce_list announce ()

let split_at n s =
  let len = String.length s in
  if len mod n <> 0 then invalid_arg "Torrent.split_at"
  else Array.init (len/n) (fun i -> String.sub s (n*i) n)

let sha1s bc =
  Bcode.find "info" bc |> Bcode.find "pieces" |> Bcode.to_string |>
  split_at 20 |> Array.map Word160.from_bin

let info_hash (bc : Bcode.t) =
  Bcode.find "info" bc |> Bcode.bencode |> Word160.digest_of_string

let piece_length bc =
  Bcode.find "info" bc |> Bcode.find "piece length" |> Bcode.to_int

let name bc =
  Bcode.find "info" bc |> Bcode.find "name" |> Bcode.to_string

let files bc =
  let info = Bcode.find "info" bc in
  let name = Bcode.find "name" info |> Bcode.to_string in
  let many_files () =
    Bcode.find "files" info |> Bcode.to_list |>
    List.map (fun d ->
        let file_size = Bcode.find "length" d |> Bcode.to_int64 in
        let path = Bcode.find "path" d |> Bcode.to_list |> List.map Bcode.to_string in
        {file_size; file_path = name :: path})
  in
  let single_file () =
    let n = Bcode.find "length" info |> Bcode.to_int64 in
    [{file_size = n; file_path = [name]}]
  in
  either many_files single_file ()

let total_length (bc : Bcode.t) =
  let info = Bcode.find "info" bc in
  let many_files () =
    Bcode.find "files" info |> Bcode.to_list |>
    List.fold_left (fun acc d -> Bcode.find "length" d |> Bcode.to_int64 |> Int64.add acc) 0L
  in
  let single_file () =
    Bcode.find "length" info |> Bcode.to_int64
  in
  either single_file many_files ()

let pieces piece_length (total_length : int64) sha1s =
  let dummy_piece =
    { piece_offset = 0L; piece_length = 0; piece_digest = Word160.zero }
  in
  let no_pieces = Array.length sha1s in
  let pieces = Array.create no_pieces dummy_piece in
  let piece_length' = Int64.of_int piece_length in
  let len = ref total_length in
  let off = ref 0L in
  for i = 0 to no_pieces-1 do
    if Int64.(compare !len piece_length') < 0 then
      if i < no_pieces-1 then
        failwith "Info.pieces: small piece before last one found"
      else
        pieces.(i) <-
          { piece_offset = !off;
            piece_digest = sha1s.(i);
            piece_length = Int64.to_int !len }
    else begin
      pieces.(i) <-
        { piece_offset = !off;
          piece_digest = sha1s.(i);
          piece_length = piece_length };
      len := Int64.(sub !len piece_length');
      off := Int64.(add !off piece_length')
    end
  done;
  pieces

let rarest_piece_jitter = 42

let end_game_completion_ratio = 0.95
  
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
  let comment = comment bc in
  let sha1s = sha1s bc in
  let info_hash = info_hash bc in
  let piece_length = piece_length bc in
  let total_length = total_length bc in
  let announce_list = announce_list bc in
  let pieces = pieces piece_length total_length sha1s in
  let files = files bc in
  let fmt = Format.err_formatter in
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "             name: %s@," name;
  Format.fprintf fmt "        info hash: %s@," (Word160.to_hex info_hash);
  Format.fprintf fmt "    announce-list: @[<v>%a@]@," pp_announce_list announce_list;
  Format.fprintf fmt "     total length: %s@," (Util.string_of_file_size total_length);
  Format.fprintf fmt "     piece length: %s@," (Util.string_of_file_size (Int64.of_int piece_length));
  Format.fprintf fmt " number of pieces: %d@," (Array.length pieces);
  Format.fprintf fmt "            files: @[<v>%a@]" pp_files files;
  Format.fprintf fmt "@]@.";
  { name; comment; info_hash; announce_list; piece_length;
    total_length; pieces; files }
