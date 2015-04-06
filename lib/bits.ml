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
  { mutable data : bytes;
    mutable length : int }

let _get s i =
  Bytes.get s i <> '\x00'

let _set s i b =
  Bytes.set s i (if b then '\x01' else '\x00')

let create length =
  { data = Bytes.make length '\x00';
    length; }

let resize b i =
  if i > b.length then
    if i <= Bytes.length b.data then
      b.length <- i
    else begin
      let new_size =
        let rec loop n = if i <= n then n else loop (2 * n + 1) in
        loop (Bytes.length b.data)
      in
      let new_data = Bytes.make new_size '\x00' in
      Bytes.blit b.data 0 new_data 0 (Bytes.length b.data);
      b.data <- new_data;
      b.length <- i
    end

let set_length b size =
  if size < 0 then invalid_arg "Bits.set_length";
  if size <= Bytes.length b.data then
    b.length <- size
  else
    resize b size

let length b =
  b.length

let clear b =
  Bytes.fill b.data 0 b.length '\x00'

let set_all b =
  Bytes.fill b.data 0 b.length '\x01'

let copy b =
  { b with data = Bytes.sub b.data 0 b.length }

let count_ones b =
  let c = ref 0 in
  for i = 0 to b.length - 1 do
    if _get b.data i then incr c
  done;
  !c

let count_zeroes b =
  b.length - count_ones b

let set b i =
  if i < 0 || i >= b.length then invalid_arg "Bits.set";
  _set b.data i true

let unset b i =
  if i < 0 || i >= b.length then invalid_arg "Bits.unset";
  _set b.data i false

let is_set b i =
  if i < 0 || i >= b.length then invalid_arg "Bits.is_set";
  _get b.data i

let of_cstruct cs =
  let data = Bytes.create (Cstruct.len cs * 8) in
  for i = 0 to Cstruct.len cs - 1 do
    let n = Cstruct.get_uint8 cs i in
    for j = 0 to 7 do
      _set data (i * 8 + j) @@ (0 <> (n lsr (7 - j)) land 1)
    done
  done;
  { data; length = Cstruct.len cs * 8 }

let to_cstruct b =
  let len = (b.length + 7) / 8 in
  let cs = Cstruct.create len in
  for i = 0 to len - 1 do
    let rec loop acc j =
      if j >= 8 then
        Cstruct.set_uint8 cs i acc
      else
      if i * 8 + j < b.length && _get b.data (i * 8 + j) then
        loop (acc lor (1 lsl (7 - j))) (j + 1)
      else
        loop acc (j + 1)
    in
    loop 0 0
  done;
  cs

let has_all b =
  count_zeroes b = 0

let blit b1 o1 b2 o2 l =
  if o1 < 0 || o1 + l > b1.length || o2 < 0 || o2 + l > b2.length then invalid_arg "Bits.blit";
  Bytes.blit b1.data o1 b2.data o2 l
