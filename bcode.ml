type t =
  | BInt of int64
  | BString of string
  | BList of t list
  | BDict of (string * t) list

let find (s : string) (bc : t) : t option =
  match bc with
  | BDict d ->
    begin try Some (List.assoc s d)
    with Not_found -> None
    end
  | _ -> None

let to_list = function
  | BList l -> Some l
  | _ -> None

let to_int64 = function
  | BInt n -> Some n
  | _ -> None

let to_int = function
  | BInt n ->
    if Int64.(compare n (of_int (to_int n))) = 0 then Some (Int64.to_int n)
    else None
  | _ -> None

let to_string = function
  | BString s -> Some s
  | _ -> None

(** Bcode parsing *)

open Parser

let bint =
  wrapped (char 'i') any_int64 (char 'e') >|= fun n -> BInt n

let bstring' =
  any_int >>= fun n -> char ':' >> string_of_length n

let bstring =
  bstring' >|= fun s -> BString s

let rec blist' () =
  wrapped (char 'l') (many (fix bitem')) (char 'e') >|= fun l -> BList l

and bdict' () =
  wrapped
    (char 'd')
    (many (pair bstring' (fix bitem')))
    (char 'e') >|= fun items -> BDict items

and bitem' () =
  bint <|> fix blist' <|> bstring <|> fix bdict'

let blist = fix blist'
let bdict = fix bdict'
let bitem = fix bitem'

let bencode item =
  let b = Buffer.create 17 in
  let rec loop = function
  | BInt n -> Printf.bprintf b "i%se" (Int64.to_string n)
  | BString s -> Printf.bprintf b "%d:%s" (String.length s) s
  | BList l ->
      Buffer.add_char b 'l';
      List.iter loop l;
      Buffer.add_char b 'e'
  | BDict d ->
      Buffer.add_char b 'd';
      List.iter (fun (k, v) ->
        Printf.bprintf b "%d:%s" (String.length k) k;
        loop v) d;
      Buffer.add_char b 'e'
  in loop item; Buffer.contents b

let test_bdecode () =
  let ints =
    QCheck.Arbitrary.(map small_int (fun n -> BInt (Int64.of_int n))) in
  let strings =
    QCheck.Arbitrary.(map string (fun s -> BString s)) in
  let any =
    QCheck.Arbitrary.(fix ~max:4 ~base:(choose [ints; strings])
      (fun arb ->
        let lists = map (list arb) (fun l -> BList l) in
        let dicts = map (list (pair string arb)) (fun d -> BDict d) in
        choose [lists; dicts])) in
  let qc = QCheck.mk_test ~n:100
    ~name:"bencoding" any
    (fun item -> run_parser (bencode item) bitem = item) in
  QCheck.run qc

let from_file (path : string) : t =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let s = String.create len in
  really_input ic s 0 len;
  run_parser s bitem

let from_string (s : string) : t =
  run_parser s bitem

(* let _ = *)
(*   test_bdecode () *)
