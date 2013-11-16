module PeerId = struct
  type t = string
  let to_string x = x
  let of_string x =
    if String.length x <> 20 then invalid_arg "PeerId.of_string";
    x
  let of_input_channel ic =
    let buf = String.create 20 in
    Lwt.bind (Lwt_io.read_into_exactly ic buf 0 20)
      (fun _ -> Lwt.return buf)
end

type peer_id = PeerId.t

module Digest = struct
  type t =
    string
  let zero =
    String.make 20 '\000'
  let compare (s1 : t) (s2 : t) =
    compare s1 s2
  let equal (s1 : t) (s2 : t) =
    s1 = s2
  let hash (s : t) =
    Hashtbl.hash s
  let to_string s =
    let buf = Buffer.create 40 in
    for i = 0 to 19 do
      Printf.bprintf buf "%02x" (int_of_char s.[i])
    done;
    Buffer.contents buf
  let pp fmt s =
    Format.fprintf fmt "%s" (to_string s)
  let to_bin x =
    x
  let from_bin x =
    if String.length x <> 20 then invalid_arg "Digest.from_bin";
    x
  let of_input_channel ic =
    let buf = String.create 20 in
    Lwt.bind (Lwt_io.read_into_exactly ic buf 0 20)
      (fun _ -> Lwt.return buf)
  let string s =
    Sha1.to_bin (Sha1.string s)
end

type digest =
  Digest.t

type state =
  | Seeding
  | Leeching

type piece_info = {
  piece_offset  : int64;
  piece_length  : int;
  piece_digest  : digest
}

type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t = {
  name          : string;
  info_hash     : digest;
  announce_list : Uri.t list list;
  pieces        : piece_info array;
  piece_length  : int;
  total_length  : int64;
  files         : file_info list
} 

let bytes_left have (pieces : piece_info array) =
  Bits.fold_left_i (fun acc i has ->
    if has then Int64.add acc (Int64.of_int pieces.(i).piece_length)
    else acc) 0L have

(* let comment bc = *)
(*   Bcode.search_string "comment" bc *)
(*  *)
(* let creation_date bc = *)
(*   Bcode.search_string "creation date" bc *)

let announce_list bc =
  let open Bcode in
  let open Option in
  let announce_list () =
    find "announce-list" bc >>= to_list >>=
    map (fun l -> to_list l >>= map (fun s -> to_string s >|= Uri.of_string))
  in
  let announce () =
    find "announce" bc >>= to_string >>= fun announce ->
    return [[Uri.of_string announce]]
  in
  either announce_list announce ()

let split_at n s =
  let len = String.length s in
  if len mod n <> 0 then Option.fail ()
  else Option.return (Array.init (len/n) (fun i -> String.sub s (n*i) n))

let sha1s bc =
  let open Option in
  Bcode.(find "info" bc >>= find "pieces" >>= to_string >>= split_at 20)

let info_hash (bc : Bcode.t) =
  let open Option in
  Bcode.find "info" bc >>= fun info ->
  return (Sha1.to_bin (Sha1.string (Bcode.bencode info)))

let piece_length bc =
  let open Option in
  Bcode.(find "info" bc >>= find "piece length" >>= to_int)

let name bc =
  let open Option in
  Bcode.(find "info" bc >>= find "name" >>= to_string)

let files bc =
  let open Option in
  let open Bcode in
  find "info" bc >>= fun info ->
  find "name" info >>= to_string >>= fun name ->
  let many_files () =
    find "files" info >>= to_list >>=
    map (fun d ->
      find "length" d >>= to_int64 >>= fun file_size ->
      find "path" d >>= to_list >>= map to_string >>= fun path ->
      return {file_size; file_path = name :: path})
  in
  let single_file () =
    find "length" info >>= to_int64 >>= fun n ->
    return [{file_size = n; file_path = [name]}]
  in
  either many_files single_file ()

let total_length (bc : Bcode.t) =
  let open Option in
  let open Bcode in
  find "info" bc >>= fun info ->
  let many_files () =
    find "files" info >>= to_list >>=
    fold (fun acc d -> find "length" d >>= to_int64 >|= Int64.add acc) 0L
  in
  let single_file () =
    find "length" info >>= to_int64
  in
  either single_file many_files ()

let dummy_piece = {
  piece_offset = 0L;
  piece_length = 0;
  piece_digest = Digest.zero
}

let pieces piece_length (total_length : int64) sha1s =
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

let make bc =
  let open Option in
  let info =
    name bc >>= fun name ->
    sha1s bc >>= fun sha1s ->
    info_hash bc >>= fun info_hash ->
    piece_length bc >>= fun piece_length ->
    total_length bc >>= fun total_length ->
    announce_list bc >>= fun announce_list ->
    let pieces = pieces piece_length total_length sha1s in
    files bc >>= fun files ->
    return
      { name; info_hash; announce_list; piece_length;
        total_length; pieces; files }
  in
  match info with
  | Some info -> info
  | None -> failwith "error while parsing torrent metainfo file"

let gen_peer_id () =
  let header = "-OC" ^ "0001" ^ "-" in
  let random_digit () = char_of_int ((Random.int 10) + (int_of_char '0')) in
  let random_string len =
    let s = String.create len in
    let rec loop i =
      if i >= len then s else (s.[i] <- random_digit (); loop (i+1))
    in loop 0 in
  let pid = header ^ random_string (20 - String.length header) in
  (* Lwt_log.ign_debug_f "Generated Peer ID: %s" pid; *)
  pid

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

let pp_info fmt info =
  fprintf fmt "             name: %s@," info.name;
  fprintf fmt "        info hash: %s@," (Digest.to_string info.info_hash);
  fprintf fmt "    announce-list: @[<v>%a@]@,"
    pp_announce_list info.announce_list;
  fprintf fmt "     total length: %s@," (Util.string_of_file_size info.total_length);
  fprintf fmt "     piece length: %s@," (Util.string_of_file_size (Int64.of_int info.piece_length));
  fprintf fmt " number of pieces: %d@," (Array.length info.pieces);
  fprintf fmt "            files: @[<v>%a@]" pp_files info.files

let pp ?fmt:(fmt=Format.std_formatter) info =
  fprintf fmt "@[<v>%a@]@." pp_info info
