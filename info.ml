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
  (* comment : string option; *)
  info_hash : Word160.t;
  (* announce_list : Uri.t list list; *)
  pieces : piece_info array;
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

let sha1s bc =
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
  (* let comment = comment bc in *)
  let sha1s = sha1s bc in
  let info_hash = info_hash bc in
  let piece_length = piece_length bc in
  let total_length = total_length bc in
  (* let announce_list = announce_list bc in *)
  let pieces = pieces piece_length total_length sha1s in
  let files = files bc in
  let fmt = Format.err_formatter in
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "             name: %s@," name;
  Format.fprintf fmt "        info hash: %s@," (Word160.to_hex info_hash);
  (* Format.fprintf fmt "    announce-list: @[<v>%a@]@," pp_announce_list announce_list; *)
  Format.fprintf fmt "     total length: %s@," (Util.string_of_file_size total_length);
  Format.fprintf fmt "     piece length: %s@," (Util.string_of_file_size (Int64.of_int piece_length));
  Format.fprintf fmt " number of pieces: %d@," (Array.length pieces);
  Format.fprintf fmt "            files: @[<v>%a@]" pp_files files;
  Format.fprintf fmt "@]@.";
  { name; (* comment;  *)info_hash; (* announce_list;  *)piece_length;
    total_length; pieces; files; encoded = "" (* FIXME *) }

type info_in_progress = {
  info_length : int;
  numpieces : int;
  requested : Bits.t;
  completed : Bits.t;
  pieces : string array
}

type partial_status =
  | NOSIZE
  | GOTSIZE of info_in_progress

type partial = {
  info_hash : Word160.t;
  mutable status : partial_status;
  on_completion : t -> unit
}

let of_info_hash info_hash on_completion =
  { info_hash; on_completion; status = NOSIZE }

let got_info_length nfo l =
  match nfo.status with
  | NOSIZE ->
    let numpieces = roundup l info_piece_size / info_piece_size in
    let last_piece_size = l mod info_piece_size in
    Trace.infof "Got info length: %d (%d pieces), last piece: %d" l numpieces
      last_piece_size;
    nfo.status <- GOTSIZE
        { info_length = l; completed = Bits.create numpieces;
          requested = Bits.create numpieces; numpieces;
          pieces = Array.init numpieces (fun i ->
              if i < numpieces-1 then String.make info_piece_size '\000'
              else String.make last_piece_size '\000') };
    true
  | GOTSIZE _ ->
    false

let request_lost nfo i =
  match nfo.status with
  | NOSIZE ->
    ()
  | GOTSIZE gs ->
    Trace.infof "Info: piece request lost: %d" i;
    Bits.unset gs.requested i

let next_piece nfo =
  match nfo.status with
  | NOSIZE -> None
  | GOTSIZE gs ->
    let interesting = Bits.lognot (Bits.logor gs.completed gs.requested) in
    (* let interesting = Bits.lognot gs.completed in *)
    Trace.infof "Info: next_piece: count: %d length: %d" (Bits.count interesting)
      (Bits.length interesting);
    let rec loop i =
      if i >= Bits.length interesting then None
      else if Bits.is_set interesting i then begin
        Bits.set gs.requested i;
        Trace.infof "Info: requesting info piece %d" i;
        Some i
      end else loop (i+1)
    in
    loop 0

let got_piece nfo i s =
  match nfo.status with
  | NOSIZE ->
    assert false
  | GOTSIZE gs ->
    if not (Bits.is_set gs.completed i) then begin
      gs.pieces.(i) <- s;
      Bits.set gs.completed i;
      Trace.infof "Got info piece %d" i;
      if Bits.count gs.completed = gs.numpieces then
        Trace.infof "All info pieces received!";
      let s = String.concat "" (Array.to_list gs.pieces) in
      if Word160.equal (Word160.digest_of_string s) nfo.info_hash then begin
        Trace.infof "Info data validated!";
        let bc = Get.run Bcode.bdecode s in
        nfo.on_completion (create bc)
      end else begin
        Trace.infof "Info data invalid!";
        nfo.status <- NOSIZE
      end
    end
