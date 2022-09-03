type file = { length : int; path : string }

type t = {
  announce : string;
  comment : string;
  files : file list;
  piece_length : int;
  pieces : string array;
  info_hash : string;
}

let to_sexp t =
  let open Sexp.Encoder in
  let file t = record [ ("length", int t.length); ("path", string t.path) ] in
  record
    [
      ("announce", string t.announce);
      ("comment", string t.comment);
      ("files", list file t.files);
      ("piece_length", int t.piece_length);
      ("pieces", array string t.pieces);
      ("info_hash", string t.info_hash);
    ]

let decoder =
  let open Bencode.Decoder in
  let open Bencode.Decoder.O in
  let+ announce = member "announce" string
  and+ comment = member "comment" string
  and+ name = member "info" (member "name" string)
  and+ files =
    let file =
      let+ length = member "length" int
      and+ path = member "path" (list string) in
      (length, path)
    in
    member "info"
      (if_member "length"
         (let+ length = int in
          `Single length)
         (let+ files = member "files" (list file) in
          `Multiple files))
  and+ piece_length = member "info" (member "piece length" int)
  and+ pieces = member "info" (member "pieces" string)
  and+ info_hash =
    member "info"
      (let+ v = value in
       Sha1.to_bin (Sha1.string (Bencode.encode v)))
  in
  let pieces =
    let hash_size = 20 in
    assert (String.length pieces mod hash_size = 0);
    Array.init
      (String.length pieces / hash_size)
      (fun i -> String.sub pieces (i * hash_size) hash_size)
  in
  let files =
    match files with
    | `Single length -> [ { length; path = name } ]
    | `Multiple files ->
        List.map
          (fun (length, path) ->
            { length; path = String.concat "/" (name :: path) })
          files
  in
  { announce; comment; files; piece_length; pieces; info_hash }

let length t =
  List.fold_left (fun accu { length; _ } -> accu + length) 0 t.files

let piece_length t i = min t.piece_length (length t - (t.piece_length * i))

let piece_offset t i =
  i * t.piece_length
