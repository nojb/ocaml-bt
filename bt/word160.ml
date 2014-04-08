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
  string
    
let zero =
  String.make 20 '\000'
    
let compare s1 s2 =
  compare s1 s2
    
let equal s1 s2 =
  s1 = s2
  
let hash (s : t) =
  Hashtbl.hash s
    
let to_hex s =
  let buf = Buffer.create 40 in
  for i = 0 to 19 do
    Printf.bprintf buf "%02x" (int_of_char s.[i])
  done;
  Buffer.contents buf

let sprint () s =
  to_hex s

let to_hex_short s =
  let s = to_hex s in
  (* String.sub s 0 6 *)
  Printf.sprintf "%s...%s" (String.sub s 0 5) (String.sub s 15 5)
    
let pp fmt s =
  Format.fprintf fmt "%s" (to_hex s)

let print oc s =
  output_string oc s

let sprint () s =
  to_hex s
    
let to_bin x =
  x
    
let from_bin x =
  if String.length x <> 20 then invalid_arg "Word160.from_bin";
  x
      
let digest_of_string s =
  Sha1.to_bin (Sha1.string s)
    
let to_z s =
  Z.of_bits s

let dist s1 s2 : Z.t =
  Z.(logxor (of_bits s1) (of_bits s2))

let random () =
  let s = String.create 20 in
  for i = 0 to 19 do
    s.[i] <- Char.chr (Random.int 256)
  done;
  s

let peer_id prefix =
  let prefix =
    if String.length prefix > 20 then String.sub prefix 0 20
    else prefix
  in
  let random_digit () =
    char_of_int ((Random.int 10) + (int_of_char '0'))
  in
  let random_string len =
    let s = String.create len in
    let rec loop i =
      if i >= len then s
      else begin
        s.[i] <- random_digit ();
        loop (i+1)
      end
    in
    loop 0
  in
  prefix ^ random_string (20 - String.length prefix)

let unhex_char = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' | 'A' -> 10
  | 'b' | 'B' -> 11
  | 'c' | 'C' -> 12
  | 'd' | 'D' -> 13
  | 'e' | 'E' -> 14
  | 'f' | 'F' -> 15
  | _ -> invalid_arg "Word160.unhex_char"

let of_hex s =
  let l = String.length s in
  if l <> 40 then invalid_arg "Word160.of_hex";
  let s' = String.create 20 in
  for i = 19 downto 0 do
    let c1 = unhex_char s.[2*i+1] in
    let c2 = unhex_char s.[2*i] in
    s'.[i] <- Char.chr (c1 + (c2 lsl 4))
  done;
  s'

let unbase32_char c =
  match c with
  | 'A' .. 'Z' -> Char.code c - Char.code 'A'
  | '2' .. '7' -> Char.code c - Char.code '2' + 26
  | _ -> invalid_arg "Word160.unbase32_char"

let of_base32 s =
  let l = String.length s in
  if l <> 32 then invalid_arg "Word160.of_base32";
  let rec loop acc i =
    if i >= 32 then acc
    else loop (Z.add acc (Z.shift_left (Z.of_int (unbase32_char s.[31-i])) (5*i))) (i+1)
  in
  let z = loop Z.zero 0 in
  let bits = Z.to_bits z in
  let s' = String.make 20 '\000' in
  for i = 0 to 19 do
    s'.[i] <- bits.[19-i]
  done;
  s'

let pp fmt s =
  Format.fprintf fmt "0x%s" (to_hex s)
