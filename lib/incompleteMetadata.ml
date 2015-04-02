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
  { info_hash : SHA1.t;
    length : int;
    pieces : Bits.t;
    raw : Cstruct.t }

let metadata_block_size = 1 lsl 14
let metadata_max_size = 1 lsl 22

let create ~info_hash ~length =
  let size = (length + metadata_block_size - 1) / metadata_block_size in
  { info_hash; length; pieces = Bits.create size; raw = Cstruct.create length }

let add m n buf =
  if n < 0 || n >= Bits.length m.pieces then invalid_arg "add";
  Bits.set m.pieces n;
  Cstruct.blit buf 0 m.raw (n * metadata_block_size) (Cstruct.len buf);
  match Bits.has_all m.pieces with
  | true ->
      if SHA1.(equal (digest m.raw) m.info_hash) then
        `Verified m.raw
      else
        `Failed
  | false ->
      `More

let iter_missing f m =
  for i = 0 to Bits.length m.pieces - 1 do
    if not (Bits.is_set m.pieces i) then
      f i
  done
