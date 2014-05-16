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

type t =
  | Int of int64
  | String of string
  | List of t list
  | Dict of (string * t) list

let find (s : string) (bc : t) : t =
  match bc with
  | Dict d ->
    List.assoc s d
  | _ ->
    invalid_arg (Printf.sprintf "Bcode.find: key %S not found" s)

let to_list = function
  | List l -> l
  | _ -> invalid_arg "Bcode.to_list"

let to_int64 = function
  | Int n -> n
  | _ -> invalid_arg "Bcode.to_int64"

let to_int = function
  | Int n ->
    if Int64.(compare n (of_int (to_int n))) = 0 then Int64.to_int n
    else invalid_arg "Bcode.to_int"
  | _ ->
    invalid_arg "Bcode.to_int"

let to_string = function
  | String s -> s
  | _ -> invalid_arg "Bcode.to_string"

let to_dict = function
  | Dict d -> d
  | _ -> invalid_arg "Bcode.to_dict"

(** Bcode parsing *)

let decode_partial s =
  let len = String.length s in
  let rec loop pos =
    assert (pos < len);
    match s.[pos] with
    | 'i' ->
      let start = pos+1 in
      let rec loop1 pos =
        assert (pos < len);
        match s.[pos] with
        | 'e' -> Int (Int64.of_string (String.sub s start (pos-start))), pos+1
        | _ -> loop1 (pos+1)
      in
      loop1 start
    | 'l' ->
      let start = pos+1 in
      let rec loop1 pos =
        assert (pos < len);
        match s.[pos] with
        | 'e' -> [], pos+1
        | _ ->
          let x, pos = loop pos in
          let xs, pos = loop1 pos in
          x :: xs, pos
      in
      let xs, pos = loop1 start in
      List xs, pos
    | '0' .. '9' ->
      let start = pos in
      let rec loop1 pos =
        assert (pos < len);
        match s.[pos] with
        | '0' .. '9' ->
          loop1 (pos+1)
        | ':' ->
          let n = int_of_string (String.sub s start (pos-start)) in
          String (String.sub s (pos+1) n), pos+n+1
        | _ ->
          assert false
      in
      loop1 (start+1)
    | 'd' ->
      let start = pos+1 in
      let rec loop1 pos =
        assert (pos < len);
        match s.[pos] with
        | 'e' -> [], pos+1
        | _ ->
          match loop pos with
          | String k, pos ->
            let v, pos = loop pos in
            let d, pos = loop1 pos in
            (k, v) :: d, pos
          | _ -> assert false
      in
      let d, pos = loop1 start in
      Dict d, pos
    | _ ->
      assert false
  in
  loop 0

let decode s =
  let bc, _ = decode_partial s in
  bc

let encode item =
  let b = Buffer.create 17 in
  let rec loop = function
    | Int n ->
      Buffer.add_char b 'i';
      Buffer.add_string b (Int64.to_string n);
      Buffer.add_char b 'e'
    | String s ->
      Buffer.add_string b (string_of_int (String.length s));
      Buffer.add_char b ':';
      Buffer.add_string b s
    | List l ->
      Buffer.add_char b 'l';
      List.iter loop l;
      Buffer.add_char b 'e'
    | Dict d ->
      Buffer.add_char b 'd';
      List.iter (fun (k, v) ->
          Buffer.add_string b (string_of_int (String.length k));
          Buffer.add_char b ':';
          Buffer.add_string b k;
          loop v) d;
      Buffer.add_char b 'e'
  in
  loop item;
  Buffer.contents b

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
