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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let invalid_arg_lwt s = Lwt.fail (Invalid_argument s)

let rarest_first_cutoff = 1

type active_block = {
  a_piece : int;
  a_block : int
}

type pending_piece = {
  p_index : int;
  mutable p_reqs : int
}

type t = {
  meta : Metadata.t;
  store : Store.t;
  mutable active : active_block list;
  pending : pending_piece option array;
  completed : Bits.t array;
  mutable up : int64;
  mutable down : int64;
  mutable amount_left : int64;
  rarity : int array
}

(* number of missing blocks in piece [i] *)
let missing_blocks_in_piece t i =
  let rec loop n j =
    if j >= Bits.length t.completed.(i) then n else
    if Bits.is_set t.completed.(i) j then loop n (j+1)
    else loop (n+1) (j+1)
  in
  loop 0 0

let compare_by_weight t p1 p2 =
  match p1, p2 with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some p1, Some p2 ->
    let w p = (missing_blocks_in_piece t p.p_index, t.rarity.(p.p_index)) in
    let (m1, h1) = w p1 in
    let (m2, h2) = w p2 in
    if m2 < m1 then 1 else
    if m1 < m2 then -1 else
    if h1 < h2 then -1 else
    if h2 < h1 then 1
    else 0
  
let request_block t have =
  Array.sort (compare_by_weight t) t.pending;
  let rec loop i =
    if i >= Array.length t.pending then None
    else match t.pending.(i) with
      | None -> None
      | Some p ->
        if not (have p.p_index) then loop (i+1)
        else
          let rec loop' j =
            if j >= Bits.length t.completed.(p.p_index) then loop (i+1) else
            if List.exists (fun r -> r.a_piece = p.p_index && r.a_block = j) t.active then loop' (j+1) else
            if not (Bits.is_set t.completed.(p.p_index) j) then Some (p.p_index, j)
            else loop' (j+1)
          in
          loop' 0
  in
  match loop 0 with
  | Some (p, j) ->
    t.active <- { a_piece = p; a_block = j } :: t.active;
    Some (p, j * t.meta.Metadata.block_size, Metadata.block_size t.meta p j)
  | None ->
    None

let create meta =
  let numpieces = Array.length meta.Metadata.hashes in
  let dl = {
    meta; store = Store.create ();
    active = [];
    pending = Array.mapi (fun i _ -> Some { p_index = i; p_reqs = 0 }) meta.Metadata.hashes;
    completed =
      Array.init numpieces (fun i -> Bits.create (Metadata.block_count meta i));
    up = 0L; down = 0L;
    amount_left = meta.Metadata.total_length;
    rarity = Array.create numpieces 0
  } in
  Lwt_list.iter_s
    (fun fi -> Store.add_file dl.store fi.Metadata.file_path fi.Metadata.file_size)
    dl.meta.Metadata.files >>= fun () ->
  let piece_size = dl.meta.Metadata.piece_length in
  let total_length = dl.meta.Metadata.total_length in
  let plen i =
    if i < numpieces - 1 then piece_size
    else
      Int64.(mul (of_int i) (of_int piece_size) |>
             sub total_length |> Util.safe_int64_to_int)
  in
  let rec loop good acc i =
    if i >= numpieces then
      Lwt.return (good, acc)
    else
      let off = Int64.(mul (of_int i) (of_int piece_size)) in
      Store.read dl.store off (plen i) >>= fun s ->
      if SHA1.digest_of_string s |> SHA1.equal dl.meta.Metadata.hashes.(i) then begin
        Bits.set_all dl.completed.(i);
        dl.pending.(i) <- None;
        loop (good+1) (Int64.(sub acc (of_int (plen i)))) (i+1)
      end else
        loop good acc (i+1)
  in
  let start_time = Unix.gettimeofday () in
  loop 0 dl.meta.Metadata.total_length 0 >>= fun (good, amount_left) ->
  let end_time = Unix.gettimeofday () in
  Log.success
    "torrent initialisation complete (good=%d,total=%d,left=%Ld,secs=%.0f)"
    good numpieces amount_left (end_time -. start_time);
  dl.amount_left <- amount_left;
  Lwt.return dl

let get_block t i ofs len =
  t.up <- Int64.add t.up (Int64.of_int len);
  Store.read t.store (Metadata.block_offset t.meta i ofs) len
  
let got_have self piece =
  if not (Bits.has_all self.completed.(piece)) then
    self.rarity.(piece) <- self.rarity.(piece) + 1

let got_bitfield self b =
  for i = 0 to Bits.length b - 1 do
    if Bits.is_set b i then got_have self i
  done
 
let lost_have self piece =
  if not (Bits.has_all self.completed.(piece)) then
    self.rarity.(piece) <- self.rarity.(piece) - 1

let lost_bitfield self b =
  for i = 0 to Bits.length b - 1 do
    if Bits.is_set b i then lost_have self i
  done

let lost_request t (i, ofs, len) =
  let b = ofs / t.meta.Metadata.block_size in
  t.active <- List.filter (fun a -> a.a_piece <> i || a.a_block <> b) t.active

let got_block t idx off s =
  t.down <- Int64.add t.down (Int64.of_int (String.length s));
  if not (Bits.has_all t.completed.(idx)) then begin
    let b = off / t.meta.Metadata.block_size in
    if not (Bits.is_set t.completed.(idx) b) then begin
      Store.write t.store (Metadata.block_offset t.meta idx off) s >>= fun () ->
      Bits.set t.completed.(idx) b;
      t.active <- List.filter (fun r -> r.a_piece <> idx || r.a_block <> b) t.active;
      if Bits.has_all t.completed.(idx) then begin
        Log.success "piece %d tentatively completed" idx;
        Store.read t.store (Metadata.piece_offset t.meta idx)
          (Metadata.piece_length t.meta idx) >>= fun s ->
        if SHA1.digest_of_string s = t.meta.Metadata.hashes.(idx) then begin
          Log.success "piece verified (idx=%d)" idx;
          t.pending.(idx) <- None;
          t.amount_left <- Int64.(sub t.amount_left (of_int (Metadata.piece_length t.meta idx)));
          Lwt.return `Verified
        end else begin
          Bits.clear t.completed.(idx);
          Log.error "piece failed hashcheck (idx=%d)" idx;
          Lwt.return `Failed
        end
      end else begin
        Lwt.return `Continue
      end
    end
    else begin
      Log.info "received a block that we already have";
      Lwt.return `Continue
    end
  end else
    Lwt.return `Verified
  
let is_complete self =
  let rec loop i =
    if i >= Array.length self.completed then true else
    if Bits.has_all self.completed.(i) then loop (i+1)
    else false
  in
  loop 0
    
let down self =
  self.down

let up self =
  self.up

let amount_left self =
  self.amount_left

let numgot self =
  let rec loop n i =
    if i >= Array.length self.completed then n else
    if Bits.has_all self.completed.(i) then loop (n+1) (i+1)
    else loop n (i+1)
  in
  loop 0 0

let have self =
  let b = Bits.create (Array.length self.completed) in
  for i = 0 to (Array.length self.completed) - 1 do
    if Bits.has_all self.completed.(i) then Bits.set b i
  done;
  b

let has_piece self i =
  Bits.has_all self.completed.(i)
