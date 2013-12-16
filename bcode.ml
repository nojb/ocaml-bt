type t =
  | BInt of int64
  | BString of string
  | BList of t list
  | BDict of (string * t) list

let find (s : string) (bc : t) : t =
  match bc with
  | BDict d ->
    List.assoc s d
  | _ ->
    invalid_arg "Bcode.find"

let to_list = function
  | BList l -> l
  | _ -> invalid_arg "Bcode.to_list"

let to_int64 = function
  | BInt n -> n
  | _ -> invalid_arg "Bcode.to_int64"

let to_int = function
  | BInt n ->
    if Int64.(compare n (of_int (to_int n))) = 0 then Int64.to_int n
    else invalid_arg "Bcode.to_int"
  | _ ->
    invalid_arg "Bcode.to_int"

let to_string = function
  | BString s -> s
  | _ -> invalid_arg "Bcode.to_string"

let to_dict = function
  | BDict d -> d
  | _ -> invalid_arg "Bcode.to_dict"

(** Bcode parsing *)

let bint =
  Get.(wrapped (char 'i') any_int64 (char 'e') >|= fun n -> BInt n)

let bstring' =
  Get.(any_int >>= fun n -> char ':' >> string_of_length n)

let bstring =
  Get.(bstring' >|= fun s -> BString s)

let rec blist' () =
  Get.(wrapped (char 'l') (many (fix bitem')) (char 'e') >|= fun l -> BList l)

and bdict' () =
  Get.(wrapped
    (char 'd')
    (many (pair bstring' (fix bitem')))
    (char 'e') >|= fun items -> BDict items)

and bitem' () =
  Get.(bint <|> fix blist' <|> bstring <|> fix bdict')

let blist = Get.fix blist'
let bdict = Get.fix bdict'
let bitem = Get.fix bitem'

let bdecode = bitem

let rec bencode item =
  let open Put in
  match item with
  | BInt n ->
    char 'i' >> string (Int64.to_string n) >> char 'e'
  | BString s ->
    string (string_of_int (String.length s)) >> char ':' >> string s
  | BList l ->
    List.fold_left (fun i x -> i >> bencode x) (char 'l') l >> char 'e'
  | BDict d ->
    List.fold_left (fun i (k, v) ->
        i >> string (string_of_int (String.length k)) >> char ':' >> string k >>
        bencode v) (char 'd') d >> char 'e'
  (* let b = Buffer.create 17 in *)
  (* let rec loop = function *)
  (* | BInt n -> Printf.bprintf b "i%se" (Int64.to_string n) *)
  (* | BString s -> Printf.bprintf b "%d:%s" (String.length s) s *)
  (* | BList l -> *)
  (*     Buffer.add_char b 'l'; *)
  (*     List.iter loop l; *)
  (*     Buffer.add_char b 'e' *)
  (* | BDict d -> *)
  (*     Buffer.add_char b 'd'; *)
  (*     List.iter (fun (k, v) -> *)
  (*       Printf.bprintf b "%d:%s" (String.length k) k; *)
  (*       loop v) d; *)
  (*     Buffer.add_char b 'e' *)
  (* in loop item; Buffer.contents b *)

(* let test_bdecode () = *)
(*   let ints = *)
(*     QCheck.Arbitrary.(map small_int (fun n -> BInt (Int64.of_int n))) in *)
(*   let strings = *)
(*     QCheck.Arbitrary.(map string (fun s -> BString s)) in *)
(*   let any = *)
(*     QCheck.Arbitrary.(fix ~max:4 ~base:(choose [ints; strings]) *)
(*       (fun arb -> *)
(*         let lists = map (list arb) (fun l -> BList l) in *)
(*         let dicts = map (list (pair string arb)) (fun d -> BDict d) in *)
(*         choose [lists; dicts])) in *)
(*   let qc = QCheck.mk_test ~n:100 *)
(*     ~name:"bencoding" any *)
(*     (fun item -> Get.run_full bitem (bencode item) = item) in *)
(*   QCheck.run qc *)

(* let from_file path = *)
(*   let ic = open_in_bin path in *)
(*   let len = in_channel_length ic in *)
(*   let s = String.create len in *)
(*   really_input ic s 0 len; *)
(*   Get.run_full bitem s *)

(* let from_string (s : string) : t = *)
(*   Get.run_full bitem s *)

(* let _ = *)
(*   test_bdecode () *)
