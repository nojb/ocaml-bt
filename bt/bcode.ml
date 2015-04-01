(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

type t =
  | Int of int64
  | String of Cstruct.t
  | List of t list
  | Dict of (string * t) list

let find name = function
  | Dict d when List.mem_assoc name d ->
      List.assoc name d
  | _ ->
      invalid_arg (Printf.sprintf "Bcode.find: key %S not found" name)

let to_list = function
  | List l ->
      l
  | _ ->
      invalid_arg "Bcode.to_list"

let to_int64 = function
  | Int n -> n
  | _ -> invalid_arg "Bcode.to_int64"

let to_int = function
  | Int n ->
      Int64.to_int n
  | _ ->
      invalid_arg "Bcode.to_int"

let strl sep f oc = function
  | [] -> ()
  | [x] -> f oc x
  | x :: xs ->
      f oc x;
      List.iter (fun x -> Printf.fprintf oc "%s%a" sep f x) xs

let rec print oc = function
  | String s ->
      Printf.fprintf oc "%S" (Cstruct.to_string s) (* FIXME FIXME *)
  | Int n ->
      Printf.fprintf oc "%Ld" n
  | Dict d ->
      let aux oc (k, v) = Printf.fprintf oc "%s: %a" k print v in
      Printf.fprintf oc "{%a}" (strl ";" aux) d
  | List l ->
      Printf.fprintf oc "[%a]" (strl "," print) l

let to_string = function
  | String cs ->
      Cstruct.to_string cs
  | _ ->
      invalid_arg "Bcode.to_string"

let to_cstruct = function
  | String cs ->
      cs
  | _ ->
      invalid_arg "Bcode.to_cstruct"

let to_dict = function
  | Dict d ->
      d
  | _ ->
      invalid_arg "Bcode.to_dict"

(** Bcode parsing *)

let decode_partial cs =
  let rec loop i =
    match Char.chr @@ Cstruct.get_uint8 cs i with
    | 'i' ->
        let start = i + 1 in
        let rec loop' i =
          assert (i < Cstruct.len cs);
          match Char.chr @@ Cstruct.get_uint8 cs i with
          | 'e' ->
              Int (Int64.of_string @@ Cstruct.copy cs start (i - start)), (i + 1)
          | '0' .. '9' ->
              loop' (i + 1)
          | _ ->
              failwith "Bcode.decode_partial: bad digit"
        in
        loop' start

    | 'l' ->
        let rec loop' acc i =
          assert (i < Cstruct.len cs);
          match Char.chr @@ Cstruct.get_uint8 cs i with
          | 'e' ->
              List (List.rev acc), (i + 1)
          | _ ->
              let x, i = loop i in
              loop' (x :: acc) i
        in
        loop' [] (i + 1)

    | '0' .. '9' ->
        let start = i in
        let rec loop' i =
          assert (i < Cstruct.len cs);
          match Char.chr @@ Cstruct.get_uint8 cs i with
          | '0' .. '9' ->
              loop' (i + 1)
          | ':' ->
              let n = int_of_string @@ Cstruct.copy cs start (i - start) in
              String (Cstruct.sub cs (i + 1) n), (i + n + 1)
          | _ ->
              failwith "Bcode.decode_partial: bad string"
        in
        loop' start

    | 'd' ->
        let rec loop' acc i =
          assert (i < Cstruct.len cs);
          match Char.chr @@ Cstruct.get_uint8 cs i with
          | 'e' ->
              Dict (List.rev acc), (i + 1)
          | _ ->
              begin match loop i with
              | String k, i ->
                  let v, i = loop i in
                  loop' ((Cstruct.to_string k, v) :: acc) i
              | _ ->
                  failwith "Bcode.decode_partial: bad dict"
              end
        in
        loop' [] (i + 1)

    | _ ->
        failwith "Bcode.decode_partial: bad"
  in
  let v, i = loop 0 in
  v, Cstruct.shift cs i

let decode s =
  let bc, _ = decode_partial s in
  bc

let rec writer x =
  let open Util.W in
  match x with
  | Int n ->
      char 'i' <+> string (Int64.to_string n) <+> char 'e'
  | String cs ->
      string (string_of_int (Cstruct.len cs)) <+> char ':' <+> immediate cs
  | List l ->
      char 'l' <+> concat (List.map writer l) <+> char 'e'
  | Dict d ->
      let aux (k, v) = string (string_of_int (String.length k)) <+> char ':' <+> string k <+> writer v in
      char 'd' <+> concat (List.map aux d) <+> char 'e'

let encode x =
  Util.W.to_cstruct (writer x)

(* let encode item = *)
(*   let b = Buffer.create 17 in *)
(*   let rec loop = function *)
(*     | Int n -> *)
(*         Buffer.add_char b 'i'; *)
(*         Buffer.add_string b (Int64.to_string n); *)
(*         Buffer.add_char b 'e' *)
(*     | String s -> *)
(*         Buffer.add_string b (string_of_int (String.length s)); *)
(*         Buffer.add_char b ':'; *)
(*         Buffer.add_string b s *)
(*     | List l -> *)
(*         Buffer.add_char b 'l'; *)
(*         List.iter loop l; *)
(*         Buffer.add_char b 'e' *)
(*     | Dict d -> *)
(*         Buffer.add_char b 'd'; *)
(*         List.iter (fun (k, v) -> *)
(*           Buffer.add_string b (string_of_int (String.length k)); *)
(*           Buffer.add_char b ':'; *)
(*           Buffer.add_string b k; *)
(*           loop v) d; *)
(*         Buffer.add_char b 'e' *)
(*   in *)
(*   loop item; *)
(*   Buffer.contents b *)

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
