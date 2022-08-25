type t =
  | Int of int
  | String of string
  | List of t list
  | Dict of (string * t) list

let rec to_sexp t =
  let open Sexp.Encoder in
  match t with
  | Int n -> int n
  | String s -> string s
  | List l -> list to_sexp l
  | Dict l -> list (pair string to_sexp) l

(* TODO : check for leading zeros (invalid) *)

let rec decode s i =
  match s.[i] with
  | 'i' ->
      let rec loop accu i =
        if i = String.length s then raise Exit;
        match s.[i] with
        | 'e' -> (Int accu, i + 1)
        | '0' .. '9' as c ->
            loop ((10 * accu) + (Char.code c - Char.code '0')) (i + 1)
        | _ -> raise Exit
      in
      loop 0 (i + 1)
  | 'l' ->
      let rec loop accu i =
        if i = String.length s then raise Exit;
        match s.[i] with
        | 'e' -> (List (List.rev accu), i + 1)
        | _ ->
            let x, i = decode s i in
            loop (x :: accu) i
      in
      loop [] (i + 1)
  | '0' .. '9' ->
      let rec loop accu i =
        if i = String.length s then raise Exit;
        match s.[i] with
        | '0' .. '9' as c ->
            loop ((10 * accu) + Char.code c - Char.code '0') (i + 1)
        | ':' -> (String (String.sub s (i + 1) accu), i + 1 + accu)
        | _ -> raise Exit
      in
      loop 0 i
  | 'd' ->
      let rec loop accu i =
        if i = String.length s then raise Exit;
        match s.[i] with
        | 'e' -> (Dict (List.rev accu), i + 1)
        | _ -> (
            match decode s i with
            | String k, i ->
                let v, i = decode s i in
                loop ((k, v) :: accu) i
            | _ -> raise Exit)
      in
      loop [] (i + 1)
  | _ -> raise Exit

let decode s =
  match decode s 0 with
  | x, i -> if i = String.length s then Some x else None
  | exception Exit -> None

let rec encode buf = function
  | Int n ->
      Buffer.add_char buf 'i';
      Buffer.add_string buf (string_of_int n);
      Buffer.add_char buf 'e'
  | String s ->
      Buffer.add_string buf (string_of_int (String.length s));
      Buffer.add_char buf ':';
      Buffer.add_string buf s
  | List l ->
      Buffer.add_char buf 'l';
      List.iter (encode buf) l;
      Buffer.add_char buf 'e'
  | Dict l ->
      Buffer.add_char buf 'd';
      List.iter
        (fun (k, v) ->
          Buffer.add_string buf (string_of_int (String.length k));
          Buffer.add_char buf ':';
          Buffer.add_string buf k;
          encode buf v)
        l;
      Buffer.add_char buf 'e'

let encode t =
  let buf = Buffer.create 17 in
  encode buf t;
  Buffer.contents buf

module Decoder = struct
  type bencode = t

  type 'a t = bencode -> 'a

  let int = function Int n -> n | _ -> raise Exit

  let string = function String s -> s | _ -> raise Exit

  let list p = function List l -> List.map p l | _ -> raise Exit

  let member s p = function
    | Dict l -> (
        match List.assoc s l with x -> p x | exception Not_found -> raise Exit)
    | _ -> raise Exit

  let if_member s p q = function
    | Dict l as x -> (
        match List.assoc s l with x -> p x | exception Not_found -> q x)
    | _ -> raise Exit

  let if_list p q = function List l -> List.map p l | _ as x -> q x

  let value t = t

  let query p t = match p t with x -> Some x | exception Exit -> None

  module O = struct
    let ( let+ ) p f t = f (p t)

    let ( and+ ) p q t = (p t, q t)
  end
end
