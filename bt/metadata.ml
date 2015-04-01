(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

let section = Log.make_section "Metadata"

let debug ?exn fmt = Log.debug section ?exn fmt

let _ = Random.self_init ()

type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t = {
  name : string;
  info_hash : SHA1.t;
  hashes : SHA1.t array;
  piece_length : int;
  block_size : int;
  last_piece_size : int;
  total_length : int64;
  files : file_info list;
  encoded : Cstruct.t
}

let length info =
  Cstruct.len info.encoded

let info_piece_size = 16 * 1024 (* bytes *)

(* let roundup n r = *)
(*   (n + r - 1) / r * r *)

let get_piece info i =
  let l = Cstruct.len info.encoded in
  let numpieces = (l + info_piece_size - 1) / info_piece_size in
  if i >= numpieces || i < 0 then invalid_arg "Info.get_piece";
  if i < numpieces - 1 then
    Cstruct.sub info.encoded (i * info_piece_size) info_piece_size
  else
    let last_piece_size = l mod info_piece_size in
    Cstruct.sub info.encoded (i * info_piece_size) last_piece_size

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

let split_at n cs =
  let l = Cstruct.len cs in
  if l mod n <> 0 then invalid_arg "Metadata.split_at";
  Array.init (l / n) (fun i -> Cstruct.sub cs (n * i) n)

let hashes bc =
  Bcode.find "pieces" bc |> Bcode.to_cstruct |> split_at 20 |> Array.map SHA1.of_raw

let info_hash (bc : Bcode.t) =
  SHA1.digest (Bcode.encode bc)

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

let max_block_size = 16 * 1024

(* choose block_size so that piece_size is a multiple of block_size *)
let compute_block_size piece_size =
  let rec loop b =
    if b > max_block_size then loop (b lsr 2)
    else begin
      assert (piece_size mod b = 0);
      b
    end
  in
  loop piece_size

(* let compute_block_size _ = max_block_size *)

let create bc =
  let name = name bc in
  let hashes = hashes bc in
  let info_hash = info_hash bc in
  let piece_length = piece_length bc in
  let block_size = compute_block_size piece_length in
  debug "block_size is %d" block_size;
  let total_length = total_length bc in
  let files = files bc in
  let last_piece_size = Util.safe_int64_to_int (Int64.rem total_length (Int64.of_int piece_length)) in
  { name; info_hash; piece_length; total_length; block_size; last_piece_size;
    hashes; files; encoded = Bcode.encode bc }

let total_length m =
  m.total_length

let piece_count m =
  Array.length m.hashes

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
  Format.fprintf fmt "        info hash: %s@," (SHA1.to_hex info.info_hash);
  (* Format.fprintf fmt "    announce-list: @[<v>%a@]@," pp_announce_list announce_list; *)
  Format.fprintf fmt "     total length: %s@," (Util.string_of_file_size info.total_length);
  Format.fprintf fmt "     piece length: %s@," (Util.string_of_file_size (Int64.of_int info.piece_length));
  Format.fprintf fmt " number of pieces: %d@," (Array.length info.hashes);
  Format.fprintf fmt "            files: @[<v>%a@]" pp_files info.files;
  Format.fprintf fmt "@]@."

(* let block_size = 16 * 1024 *)

let block_count meta i =
  let len = piece_length meta i in
  (len + meta.block_size - 1) / meta.block_size

let block_size info i j =
  assert (0 <= i && i < piece_count info);
  assert (0 <= j && j < block_count info i);
  let n = piece_count info in
  if i < n - 1 then info.block_size else
  if j < block_count info i - 1 then info.block_size
  else piece_length info i mod info.block_size

let block_offset m i j =
  Int64.add (piece_offset m i) (Int64.of_int (j * m.block_size))

(* let piece_count_bytes m i = *)
(*   if i + 1 = m.piece_count then m.last_piece_size else m.piece_length *)

(* let block_count_bytes m b = *)
(*   if b + 1 = m.block_count then m.last_block_size else m.block_size *)

(* let piece_block_range m i = *)
(*   let ofs = m.piece_length * i in *)
(*   let l = ofs / m.block_size in *)
(*   let r = (ofs + piece_count_bytes m i - 1) / m.block_size in *)
(*   (l, r) *)

(* let block_location m b = *)
(*   let pos = Int64.mul (Int64.of_int b) (Int64.of_int m.block_size) in *)
(*   let pl = Int64.of_int m.piece_length in *)
(*   let i = Int64.div pos pl in *)
(*   let ofs = Int64.sub pos (Int64.mul i pl) in *)
(*   let len = block_count_bytes m b in *)
(*   (Util.safe_int64_to_int i, Util.safe_int64_to_int ofs, len) *)

(* let _block m i ofs = *)
(*   let ret = i * (m.piece_length / m.block_size) in *)
(*   ret + ofs / m.block_size *)

let hash m i =
  m.hashes.(i)

let block m i j =
  (i, j * m.block_size, block_size m i j)

let map_files m f =
  Lwt_list.map_s (fun fi -> f fi.file_path fi.file_size) m.files

let block_number m _ off =
  off / m.block_size
