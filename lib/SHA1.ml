(* The MIT License (MIT)

   Copyright (c) 2013-2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

module SHA1 = Nocrypto.Hash.SHA1
module Z    = Nocrypto.Numeric.Z
module Cs   = Nocrypto.Uncommon.Cs

let () = Random.self_init ()

let (<+>) = Cs.(<+>)

type t = Cstruct.t

let zero =
  Cstruct.of_string (String.make 20 '\000')

let last =
  Cstruct.of_string (String.make 20 '\xFF')

let compare s1 s2 =
  assert false (* FIXME FIXME *)

let equal cs1 cs2 =
  Cstruct.equal cs1 cs2

let to_raw x =
  x

let of_raw x =
  if Cstruct.len x <> 20 then invalid_arg "SHA1.of_raw";
  x

let digest = SHA1.digest
let digestv = SHA1.digestv
let to_z cs = Z.of_cstruct_be cs
let of_z z = Z.to_cstruct_be z

let distance cs1 cs2 = Z.of_cstruct_be @@ Cs.xor cs1 cs2

let generate ?(prefix = "") () =
  let prefix = if String.length prefix >= 20 then String.sub prefix 0 20 else prefix in
  let n = 20 - String.length prefix in
  let rest = Cstruct.create n in
  for i = 0 to n - 1 do Cstruct.set_uint8 rest i (Random.int 256) done;
  Cstruct.of_string prefix <+> rest

let of_hex s =
  if String.length s <> 40 then invalid_arg "SHA1.of_hex";
  Cs.of_hex s

let bprint_hex b cs =
  for i = 0 to 19 do
    Printf.bprintf b "%02x" (Cstruct.get_uint8 cs i)
  done

let sprint_hex =
  let buf = Buffer.create 40 in
  fun () cs ->
    bprint_hex buf cs;
    Buffer.contents buf

let bprint_hex_short b cs =
  for i = 19-3 to 19 do
    Printf.bprintf b "%02x" (Cstruct.get_uint8 cs i)
  done

let sprint_hex_short =
  let buf = Buffer.create 8 in
  fun () cs ->
    bprint_hex_short buf cs;
    Buffer.contents buf

let print_hex oc cs =
  for i = 0 to 19 do
    Printf.fprintf oc "%02x" (Cstruct.get_uint8 cs i)
  done

let print_hex_short oc cs =
  for i = 19-3 to 19 do
    Printf.fprintf oc "%02x" (Cstruct.get_uint8 cs i)
  done

let unbase32_char c =
  match c with
  | 'A' .. 'Z' -> Char.code c - Char.code 'A'
  | '2' .. '7' -> Char.code c - Char.code '2' + 26
  | _ -> invalid_arg "Word160.unbase32_char"

let of_base32 s =
  assert false (* FIXME FIXME *)
  (* let l = String.length s in *)
  (* if l <> 32 then invalid_arg "Word160.of_base32"; *)
  (* let rec loop acc i = *)
  (*   if i >= 32 then acc *)
  (*   else loop (Z.add acc (Z.shift_left (Z.of_int (unbase32_char s.[31-i])) (5*i))) (i+1) *)
  (* in *)
  (* let z = loop Z.zero 0 in *)
  (* let bits = Z.to_bits z in *)
  (* let s' = String.make 20 '\000' in *)
  (* for i = 0 to 19 do *)
  (*   s'.[i] <- bits.[19-i] *)
  (* done; *)
  (* s' *)
