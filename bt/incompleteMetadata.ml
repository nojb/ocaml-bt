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

type metadata_node =
  { mutable requested_at : float;
    piece : int }

type t =
  { ih : SHA1.t;
    length : int;
    raw : Cstruct.t;
    pieces_needed : metadata_node option array;
    mutable missing : int }

let kilobytes n = n * 1024

let info_piece_size = kilobytes 16 (* bytes *)

let roundup n r =
  (n + r - 1) / r * r

let create ih length =
  let npieces = roundup length info_piece_size / info_piece_size in
  let pieces_needed = Array.init npieces (fun i -> Some {requested_at = 0.0; piece = i}) in
  { ih; length; raw = Cstruct.create length; pieces_needed; missing = npieces }

let add_piece p n s =
  let rec loop i =
    match p.pieces_needed.(i) with
    | Some { piece; _ } when piece = n ->
        Cstruct.blit s 0 p.raw (n * info_piece_size) (Cstruct.len s);
        p.pieces_needed.(i) <- None;
        p.missing <- p.missing - 1
    | _ ->
        loop (i+1)
  in
  loop 0;
  p.missing = 0

let min_repeat_interval_secs = 3

let get_next_metadata_request p =
  let now = Unix.time () in
  let rec loop i =
    if i >= Array.length p.pieces_needed then None
    else match p.pieces_needed.(i) with
      | None -> loop (i+1)
      | Some pc ->
          if pc.requested_at +. float min_repeat_interval_secs < now then begin
            pc.requested_at <- now;
            Array.blit p.pieces_needed 1 p.pieces_needed 0 (Array.length p.pieces_needed - 1);
            p.pieces_needed.(Array.length p.pieces_needed - 1) <- Some pc;
            Some pc.piece
          end
          else None
  in
  loop 0

let verify p =
  if SHA1.(equal (digest p.raw) p.ih) then
    Some p.raw
  else
    None

let is_complete p =
  let rec loop i =
    if i >= Array.length p.pieces_needed then true
    else match p.pieces_needed.(i) with
      | None -> loop (i+1)
      | Some _ -> false
  in
  loop 0

let info_hash { ih } = ih

let length { length } = length

let piece_count m = Array.length m.pieces_needed
