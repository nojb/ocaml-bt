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
  let dummy =
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

type info = {
  name          : string;
  info_hash     : digest;
  (* piece_count   : int; *)
  announce_list : Uri.t list list;
  pieces        : piece_info array;
  piece_length  : int;
  total_length  : int64;
  files         : file_info list
  (* piece_length  : int; *)
  (* pieces_hash   : digest array; *)
  (* files         : file_info list; *)
} 

(* let total_size info pieces = *)
(*   Array.fold_left (fun acc piece -> Int64.add acc piece.len) 0L info.pieces *)

let bytes_left have (pieces : piece_info array) =
  let rec loop acc i =
    if i >= Array.length pieces then acc
    else if Bits.is_set i have then loop (Int64.add acc (Int64.of_int pieces.(i).piece_length)) (i+1)
    else loop acc (i+1)
  in loop 0L 0

exception Bad_format

let search_info (s : string) (bc : Bcode.t) : Bcode.t =
  Bcode.search s (Bcode.search "info" bc)

(* let comment bc = *)
(*   Bcode.search_string "comment" bc *)
(*  *)
(* let creation_date bc = *)
(*   Bcode.search_string "creation date" bc *)
(*  *)

let announce (bc : Bcode.t) : Uri.t =
  try
    Uri.of_string (Bcode.search_string "announce" bc)
  with
  | Bcode.Bad_type -> raise Bad_format

let announce_list bc : Uri.t list list =
  try
    let l = Bcode.search_list "announce-list" bc in
    List.map
      (function
        | Bcode.BList l ->
            List.map
              (function
                | Bcode.BString s -> Uri.of_string s
                | _ -> raise Bad_format) l
        | _ -> raise Bad_format) l
  with
  | Bcode.Bad_type -> raise Bad_format

let split_at n s =
  let len = String.length s in
  if len mod n <> 0 then raise Bad_format
  else Array.init (len/n) (fun i -> String.sub s (n*i) 20)

let info_pieces (bc : Bcode.t) : digest array =
  let pieces = search_info "pieces" bc in
  match pieces with
  | Bcode.BString sha1s -> split_at 20 sha1s
  | _ -> raise Bad_format

let number_pieces (bc : Bcode.t) : int =
  let pieces = search_info "pieces" bc in
  match pieces with
  | Bcode.BString sha1s ->
      let n = String.length sha1s in
      if n mod 20 <> 0 then raise Bad_format
      else n/20
  | _ -> raise Bad_format

let info_hash (bc : Bcode.t) : digest =
  Sha1.to_bin (Sha1.string (Bcode.bencode (Bcode.search "info" bc)))

let piece_length (bc : Bcode.t) : int =
  match search_info "piece length" bc with
  | Bcode.BInt n -> Int64.to_int n
  | _ -> raise Bad_format

let info_name (bc : Bcode.t) : string =
  match search_info "name" bc with
  | Bcode.BString s -> s
  | _ -> raise Bad_format

let maybe_assoc k l =
  try Some (List.assoc k l) with Not_found -> None
  
let list_assoc k l =
  match List.assoc k l with
  | Bcode.BList l -> l
  | _ -> raise Bad_format

let int_assoc k l =
  match List.assoc k l with
  | Bcode.BInt n -> n
  | _ -> raise Bad_format

let info_files (bc : Bcode.t) : file_info list =
  try
    let files = search_info "files" bc in
    match files with
    | Bcode.BList l ->
        List.map (function
          | Bcode.BDict d ->
            { file_size = int_assoc "length" d;
              file_path = List.map (function
                | Bcode.BString s -> s
                | _ -> raise Bad_format) (list_assoc "path" d) }
          | _ -> raise Bad_format) l
    | _ -> raise Bad_format
  with
  | Not_found -> (* single file mode *)
    try
      match search_info "length" bc, search_info "name" bc with
      | Bcode.BInt n, Bcode.BString name ->
          [{ file_path = [name]; file_size = n }]
      | _ -> raise Bad_format
    with
    | Not_found -> raise Bad_format

let total_length (bc : Bcode.t) : int64 =
  try
    match search_info "length" bc with
    | Bcode.BInt n -> n
    | _ -> raise Bad_format
  with
  | Not_found ->
      match search_info "files" bc with
      | Bcode.BList l ->
          begin try List.fold_left (fun acc d ->
            match d with
            | Bcode.BDict d ->
                begin match List.assoc "length" d with
                | Bcode.BInt n -> Int64.add acc n
                | _ -> raise Bad_format
                end
            | _ -> raise Bad_format) 0L l
          with
          | Not_found -> raise Bad_format
          end
       | _ -> raise Bad_format

let dummy_piece = {
  piece_offset = 0L;
  piece_length = 0;
  piece_digest = Digest.dummy
}

let extract_pieces ~piece_len:plen ~total_len:tlen ~sha1s =
  let no_pieces = Array.length sha1s in
  let a = Array.create no_pieces dummy_piece in
  let plen' = Int64.of_int plen in
  let rec loop (rlen : int64) off i =
    if i >= no_pieces then a
    else
      if Int64.compare rlen plen' < 0 then
        if i < no_pieces-1 then raise Bad_format
        else begin (* last piece *)
          assert (Int64.compare (Int64.of_int (Int64.to_int rlen)) rlen = 0);
          a.(i) <- { piece_offset = off; piece_digest = sha1s.(i); piece_length = Int64.to_int rlen };
          a
        end
      else begin
        a.(i) <- {
          piece_offset = off;
          piece_digest = sha1s.(i);
          piece_length = plen
        };
        loop (Int64.sub rlen plen') (Int64.add off plen') (i+1)
      end
  in loop tlen 0L 0
  
let make bc =
  try
    let name = info_name bc in
    let sha1s = info_pieces bc in
    let ih = info_hash bc in
    let plen = piece_length bc in
    let tlen = total_length bc in
    let alist = try announce_list bc with Not_found -> [[ announce bc ]] in
    let pieces = extract_pieces ~piece_len:plen ~total_len:tlen ~sha1s:sha1s in
    let files = info_files bc in
    { name = name; info_hash = ih; announce_list = alist; piece_length = plen;
      total_length = tlen; pieces = pieces; files = files }
  with
  | Not_found ->
      failwith "could not create torrent info"
  | Bad_format ->
      failwith "bad format torrent info"

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
