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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

open Event

type t =
  { meta : Metadata.t;
    store : Store.t;
    completed : Bits.t array;
    mutable amount_left : int64;
    handle : event -> unit }

let create meta handle =
  let numpieces = Metadata.piece_count meta in
  Store.create (Metadata.files meta) >>= fun store ->
  let dl =
    { meta; store;
      completed = Array.init numpieces (fun i -> Bits.create @@ Metadata.block_count meta i);
      amount_left = Metadata.total_length meta;
      handle }
  in
  let plen = Metadata.piece_length dl.meta in
  let rec loop good acc i =
    if i >= numpieces then
      Lwt.return (good, acc)
    else
      let off = Metadata.offset dl.meta i 0 in
      Store.read dl.store off (plen i) >>= fun s ->
      if SHA1.(equal (digest s) (Metadata.hash dl.meta i)) then begin
        Bits.set_all dl.completed.(i);
        loop (good + 1) Int64.(sub acc (of_int (plen i))) (i + 1)
      end else
        loop good acc (i+1)
  in
  (* let start_time = Unix.gettimeofday () in *)
  loop 0 (Metadata.total_length dl.meta) 0 >>= fun (good, amount_left) ->
  (* let end_time = Unix.gettimeofday () in *)
  (* debug "loaded, %d/%d good pieces, %Ld bytes left, in %.0fs" *)
  (* good numpieces amount_left (end_time -. start_time); *)
  dl.amount_left <- amount_left;
  Lwt.return dl

let get_block t i off len =
  Store.read t.store (Metadata.offset t.meta i off) len

let is_complete self =
  let rec loop i =
    (i >= Array.length self.completed) || (Bits.has_all self.completed.(i) && loop (i + 1))
  in
  loop 0

let got_block t i off s =
  (* t.down <- Int64.add t.down (Int64.of_int (String.length s)); *)
  let b = off / Metadata.block_size t.meta in
  match Bits.is_set t.completed.(i) b with
  | false ->
      Bits.set t.completed.(i) b;
      (* debug "got block %d:%d (%d remaining)" idx b *)
      (* (Bits.length t.completed.(idx) - Bits.count t.completed.(idx)); *)
      Store.write t.store (Metadata.offset t.meta i off) s >>= fun () ->
      if Bits.has_all t.completed.(i) then begin
        Store.read t.store (Metadata.offset t.meta i off)
          (Metadata.piece_length t.meta i) >>= fun s ->
        if SHA1.(equal (digest s) @@ Metadata.hash t.meta i) then begin
          t.amount_left <- Int64.(sub t.amount_left (of_int (Metadata.piece_length t.meta i)));
          t.handle (PieceVerified i);
          if is_complete t then
            Lwt.wrap1 t.handle TorrentComplete
          else
            Lwt.return_unit
        end
        else begin
          Bits.clear t.completed.(i);
          Lwt.wrap1 t.handle @@ PieceFailed i
        end
      end
      else
        Lwt.return_unit
  | ture ->
      (* debug "received a block we already have" *)
      Lwt.return_unit

let got_block t i off s =
  ignore
    (Lwt.catch (fun () -> got_block t i off s)
       (fun _ -> (* log warning *) Lwt.return_unit))

let amount_left self =
  self.amount_left

let numgot self =
  Array.fold_left (fun n b -> if Bits.has_all b then n+1 else n) 0 self.completed

let have self =
  let have = Bits.create (Array.length self.completed) in
  Array.iteri (fun i b -> if Bits.has_all b then Bits.set have i) self.completed;
  have

let has_piece self i =
  Bits.has_all self.completed.(i)

let has_block t i j _ =
  Bits.is_set t.completed.(i) (j / Metadata.block_size t.meta)

let missing_blocks_in_piece t i =
  Bits.count_zeroes t.completed.(i)

let have_size t =
  let n = ref 0L in
  for i = 0 to Array.length t.completed - 1 do
    if Bits.has_all t.completed.(i) then
      n := Int64.add !n (Int64.of_int (Metadata.piece_length t.meta i))
  done;
  !n
