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

(* let section = Log.make_section "Torrent" *)

(* let debug ?exn fmt = Log.debug section ?exn fmt *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

open Event

let invalid_arg_lwt s = Lwt.fail (Invalid_argument s)

type event_callback = event -> unit

type t = {
  meta : Metadata.t;
  store : Store.t;
  completed : Bits.t array;
  mutable up : int64;
  mutable down : int64;
  mutable amount_left : int64;
  handle : event_callback
}

let create meta handle =
  let numpieces = Metadata.piece_count meta in
  Store.create (Metadata.files meta) >>= fun store ->
  let dl = {
    meta; store;
    completed = Array.init numpieces (fun i -> Bits.create (Metadata.block_count meta i));
    up = 0L; down = 0L;
    amount_left = Metadata.total_length meta;
    handle
  } in
  let plen = Metadata.piece_length dl.meta in
  let rec loop good acc i =
    if i >= numpieces then
      Lwt.return (good, acc)
    else
      let off = Metadata.piece_offset dl.meta i in
      Store.read dl.store off (plen i) >>= fun s ->
      if SHA1.equal (SHA1.digest s) (Metadata.hash dl.meta i) then begin
        Bits.set_all dl.completed.(i);
        loop (good+1) (Int64.(sub acc (of_int (plen i)))) (i+1)
      end else
        loop good acc (i+1)
  in
  let start_time = Unix.gettimeofday () in
  loop 0 (Metadata.total_length dl.meta) 0 >>= fun (good, amount_left) ->
  let end_time = Unix.gettimeofday () in
  (* debug "loaded, %d/%d good pieces, %Ld bytes left, in %.0fs" *)
    (* good numpieces amount_left (end_time -. start_time); *)
  dl.amount_left <- amount_left;
  Lwt.return dl

let get_block t i b =
  (* FIXME Store should understand the piece/block language *)
  let _, _, l = Metadata.block t.meta i b in
  t.up <- Int64.add t.up (Int64.of_int l);
  Store.read t.store (Metadata.block_offset t.meta i b) l

let is_complete self =
  let rec loop i =
    if i >= Array.length self.completed then true else
    if Bits.has_all self.completed.(i) then loop (i+1)
    else false
  in
  loop 0

let got_block t peer idx b s =
  (* t.down <- Int64.add t.down (Int64.of_int (String.length s)); *)
  if not (Bits.is_set t.completed.(idx) b) then begin
    Bits.set t.completed.(idx) b;
    (* debug "got block %d:%d (%d remaining)" idx b *)
      (* (Bits.length t.completed.(idx) - Bits.count t.completed.(idx)); *)
    let doit () =
      Store.write t.store (Metadata.block_offset t.meta idx b) s >|= fun () ->
      if Bits.has_all t.completed.(idx) then begin
        Store.read t.store (Metadata.piece_offset t.meta idx)
          (Metadata.piece_length t.meta idx) >|= fun s ->
        if SHA1.equal (SHA1.digest s) (Metadata.hash t.meta idx) then begin
          t.amount_left <- Int64.(sub t.amount_left (of_int (Metadata.piece_length t.meta idx)));
          t.handle (PieceVerified idx);
          if is_complete t then t.handle TorrentComplete
        end
        else begin
          Bits.clear t.completed.(idx);
          t.handle (PieceFailed idx)
        end
      end
      else
        Lwt.return ()
    in
    Lwt.async doit
  end
  else
    ()
    (* debug "received a block we already have" *)

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

let has_block t i j =
  Bits.is_set t.completed.(i) j

let missing_blocks_in_piece t i =
  Bits.missing t.completed.(i)

let have_size t =
  let n = ref 0L in
  for i = 0 to Array.length t.completed - 1 do
    if Bits.has_all t.completed.(i) then
      n := Int64.add !n (Int64.of_int (Metadata.piece_length t.meta i))
  done;
  !n
