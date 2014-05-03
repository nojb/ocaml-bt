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

external get_byte: string -> int -> int = "%string_unsafe_get"
external set_byte: string -> int -> int -> unit = "%string_unsafe_set"

let create len =
  String.make len '\000'

let length v =
  String.length v

let clear v =
  String.fill v 0 (String.length v) '\000'

let set_all v =
  String.fill v 0 (String.length v) '\001'

let copy v =
  String.copy v

let count v =
  let rec loop acc i =
    if i >= String.length v then acc
    else
      if get_byte v i <> 0 then loop (acc+1) (i+1)
      else loop acc (i+1)
  in loop 0 0

let equal v1 v2 =
  let len = String.length v1 in
  if len <> String.length v2 then false
  else
    let rec loop i =
      if i >= len then true
      else
        let b1 = get_byte v1 i <> 0 in
        let b2 = get_byte v2 i <> 0 in
        if b1 && not b2 || not b1 && b2 then false
        else loop (i+1)
    in loop 0

let check_args name i len =
  if i < 0 || i >= len then invalid_arg name

let set v i =
  check_args "Bits.set" i (String.length v);
  set_byte v i 1

let unset v i =
  check_args "Bits.unset" i (String.length v);
  set_byte v i 0

let toggle v i =
  check_args "Bits.toggle" i (String.length v);
  set_byte v i (lnot (get_byte v i))

let is_set v i =
  check_args (Printf.sprintf "Bits.is_set: i=%d len=%d" i (String.length v)) i (String.length v);
  get_byte v i <> 0

let lognot v =
  let v' = String.copy v in
  for i = 0 to (String.length v)-1 do
    set_byte v' i (if get_byte v' i <> 0 then 0 else 1)
  done;
  v'

let logand v1 v2 =
  let n = String.length v1 in
  if n <> String.length v2 then invalid_arg "Bits.logand";
  let s = String.create n in
  for i = 0 to n-1 do
    set_byte s i ((get_byte v1 i) land (get_byte v2 i))
  done;
  s

let logor v1 v2 =
  let n = String.length v1 in
  if n <> String.length v2 then invalid_arg "Bits.logor";
  let s = String.create n in
  for i = 0 to n-1 do
    set_byte s i ((get_byte v1 i) lor (get_byte v2 i))
  done;
  s

let logandnot v1 v2 =
  logand v1 (lognot v2)

let iter f v =
  let n = String.length v in
  for i = 0 to n-1 do
    f (get_byte v i <> 0)
  done

let iteri f v =
  let n = String.length v in
  for i = 0 to n-1 do
    f i (get_byte v i <> 0)
  done

let map f v =
  let n = String.length v in
  let s = String.create n in
  for i = 0 to n-1 do
    set_byte s i (if f (get_byte s i <> 0) then 1 else 0)
  done;
  s

let fold_left_i f x v =
  let n = String.length v in
  let rec loop acc i =
    if i >= n then acc
    else loop (f acc i (get_byte v i <> 0)) (i+1)
  in loop x 0

let to_string v =
  let n = String.length v in
  let s = String.create n in
  for i = 0 to n-1 do
    String.unsafe_set s i (if get_byte v i <> 0 then '1' else '0')
  done;
  s

let of_bin b =
  let len = String.length b in
  let b' = String.create (len*8) in
  for i = 0 to len-1 do
    let n = get_byte b i in
    for j = 0 to 7 do
      set_byte b' (i*8 + j) ((n lsr (7-j)) land 1)
    done
  done;
  b'

let round_up n r = (n + r - 1) / r * r

let to_bin v =
  let len = String.length v in
  let len' = round_up len 8 in
  let n = len/8 in
  let b = String.make (len'/8) '\000' in
  for i = 0 to n-1 do
    let rec loop acc j =
      if j >= 8 then
        set_byte b i acc
      else if get_byte v (i*8 + j) <> 0 then
        loop (acc lor (1 lsl (7-j))) (j+1)
      else
        loop acc (j+1)
    in loop 0 0
  done;
  b

let blit src srcoff dst dstoff len =
  String.blit src srcoff dst dstoff len

let to_array v =
  Array.init (String.length v) (is_set v)

let of_array a =
  let n = Array.length a in
  let s = String.create n in
  for i = 0 to n-1 do
    s.[i] <- if a.(i) then '\001' else '\000'
  done;
  s

let to_list v =
  let rec loop i =
    if i >= String.length v then []
    else
      if is_set v i then i :: loop (i+1)
      else loop (i+1)
  in
  loop 0

let of_list l =
  let m = List.fold_left max 0 l in
  let v = String.make (m + 1) '\000' in
  List.iter (fun i -> if i >= 0 then v.[i] <- '\001' else invalid_arg "Bits.of_list") l;
  v
  
(* let arbitrary len = *)
(*   let int_multiples m n = *)
(*     QCheck.Arbitrary.(map (map n (fun x -> x / m)) (fun x -> x * m)) *)
(*   in *)
(*   QCheck.Arbitrary.(string_len (int_multiples 8 len)) *)

(* let _ = *)
(*   let test = QCheck.mk_test ~n:100 ~name:"Binary encoding/decoding" *)
(*     ~pp:to_string *)
(*     (arbitrary QCheck.Arbitrary.(int_range ~start:0 ~stop:100)) *)
(*     (fun b -> equal (of_bin (to_bin b)) b && equal (to_bin (of_bin b)) b) in *)
(*   QCheck.run test *)

let has_all b =
  let rec loop i =
    if i >= String.length b then true else
    if b.[i] = '\000' then false
    else loop (i+1)
  in
  loop 0
