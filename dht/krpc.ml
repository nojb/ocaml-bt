module Hash = Word160

type token =
  string

type trans_id =
  string

module B = Bcode

type dict =
  (string * B.t) list

type t =
  | Query of string * dict
  | Response of dict
  | Error of int64 * string

let encode (txn, msg) =
  let x = match msg with
    | Query (name, args) ->
      ["y", B.BString "q"; "q", B.BString name; "a", B.BDict args]
    | Response dict ->
      ["y", B.BString "r"; "r", B.BDict dict]
    | Error (code, s) ->
      ["y", B.BString "e"; "e", B.BList [B.BInt code; B.BString s]]
  in
  let x = ("t", B.BString txn) :: x in
  Put.run (B.bencode (B.BDict x))

let decode_exn s =
  let bc = Get.run B.bdecode s in
  let txn = B.find "t" bc |> B.to_string in
  let msg = match B.find "y" bc |> B.to_string with
  | "q" -> Query (B.find "q" bc |> B.to_string, B.find "a" bc |> B.to_dict)
  | "r" -> Response (B.find "r" bc |> B.to_dict)
  | "e" ->
    begin match B.find "e" bc |> B.to_list with
    | B.BInt n :: B.BString s :: _ -> Error (n, s)
    | _ -> failwith "decode_exn"
    end
  | _ ->
    failwith "decode_exn"
  in
  (txn, msg)
