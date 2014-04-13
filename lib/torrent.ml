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

(* type event = *)
(*   | PieceVerified of int *)
(*   | Loaded of int * int * Bits.t *)
(*   | Completed *)
                            
let invalid_arg_lwt s = Lwt.fail (Invalid_argument s)

let rarest_first_cutoff = 1

type active_piece = {
  index : int;
  blocks : [ `InProgress of int | `Done ] array;
  length : int;
  mutable missing : int
}

type t = {
  meta : Meta.t;
  store : Store.t;
  mutable active : (int * active_piece) list;
  completed : Bits.t;
  mutable up : int64;
  mutable down : int64;
  mutable amount_left : int64;
  mutable rarity : Histo.t
}

let standard_block_length = 16 * 1024

let choose_block_in_piece ~endgame t a =
  if endgame then
    let rec loop min i =
      if i >= Array.length a.blocks then
        match min with
        | None -> None
        | Some (i, _) -> Some i
      else match a.blocks.(i) with
        | `InProgress n ->
          begin match min with
          | Some (_, j) ->
            if j > n then loop (Some (i, n)) (i+1) else loop min (i+1)
          | None ->
            loop (Some (i, n)) (i+1)
          end
        | `Done ->
          loop min (i+1)
    in
    match loop None 0 with
    | Some i ->
      begin match a.blocks.(i) with
      | `InProgress n -> a.blocks.(i) <- `InProgress (n+1)
      | `Done -> assert false
      end;
      Some i
    | None ->
      None
  else
    let rec loop i =
      if i >= Array.length a.blocks then None
      else match a.blocks.(i) with
        | `InProgress 0 -> a.blocks.(i) <- `InProgress 1; Some i
        | _ -> loop (i+1)
    in
    loop 0

let request_block_in_piece ~endgame t a =
  match choose_block_in_piece ~endgame t a with
  | Some i ->
    let start = i * standard_block_length in
    Some (a.index, start, min standard_block_length (a.length - start))
  | None ->
    None

let request_piece t have =
  let is_interesting i =
    have i && not (Bits.is_set t.completed i) && not (List.mem_assoc i t.active)
  in
  let r = Histo.pick is_interesting t.rarity in
  if List.length r > 0 then
    let i = List.nth r (Random.int (List.length r)) in
    Some i
  else
    None
  
let request_block t have =
  let rec loop ~endgame = function
    | [] ->
      None
    | (i, a) :: act ->
      if have i then
        match request_block_in_piece ~endgame t a with
        | None -> loop ~endgame act
        | Some _ as b -> b
      else
        loop ~endgame act
  in
  match loop ~endgame:false t.active with
  | None ->
    begin match request_piece t have with
    | None ->
      loop ~endgame:true t.active
    | Some pc ->
      let len = Meta.piece_length t.meta pc in
      let nblocks = Meta.block_count t.meta pc in
      let a =
        {index = pc; blocks = Array.create nblocks (`InProgress 0); length = len; missing = nblocks}
      in
      t.active <- (pc, a) :: t.active;
      request_block_in_piece ~endgame:false t a
    end
  | Some _ as b -> b

let create meta =
  let npieces = Array.length meta.Meta.hashes in
  { meta; store = Store.create ();
    active = [];
    completed = Bits.create npieces;
    up = 0L; down = 0L;
    amount_left = meta.Meta.total_length;
    rarity = Histo.empty }
    
let update dl =
  Lwt_list.iter_s
    (fun fi -> Store.add_file dl.store fi.Meta.file_path fi.Meta.file_size)
    dl.meta.Meta.files >>= fun () ->
  let numpieces = Array.length dl.meta.Meta.hashes in
  let piece_size = dl.meta.Meta.piece_length in
  let total_length = dl.meta.Meta.total_length in
  let plen i =
    if i < numpieces - 1 then piece_size
    else
      Int64.(mul (of_int i) (of_int piece_size) |>
             sub total_length |> Util.safe_int64_to_int)
  in
  let rec loop acc i =
    if i >= numpieces then
      Lwt.return acc
    else
      let off = Int64.(mul (of_int i) (of_int piece_size)) in
      Store.read dl.store off (plen i) >>= fun s ->
        if Word160.digest_of_string s |> Word160.equal dl.meta.Meta.hashes.(i) then begin
          Bits.set dl.completed i;
          loop (Int64.(sub acc (of_int (plen i)))) (i+1)
        end else
          loop acc (i+1)
  in
  let start_time = Unix.gettimeofday () in
  loop dl.meta.Meta.total_length 0 >>= fun amount_left ->
  let end_time = Unix.gettimeofday () in
  Log.success
    "torrent initialisation complete (good=%d,total=%d,left=%Ld,secs=%.0f)"
    (Bits.count dl.completed) numpieces amount_left (end_time -. start_time);
  dl.amount_left <- amount_left;
  Lwt.return (Bits.count dl.completed, numpieces, dl.completed)

let get_block t i ofs len =
  Store.read t.store (Meta.block_offset t.meta i ofs) len
  
let got_have self piece =
  if not (Bits.is_set self.completed piece) then begin
    self.rarity <- Histo.add piece self.rarity;
    true
  end else
    false

let got_bitfield self b =
  let rec loop interested i =
    if i >= Bits.length b then interested
    else if Bits.is_set b i then loop (got_have self i) (i+1)
    else loop interested (i+1)
  in
  loop false 0
 
let lost_have self piece =
  if not (Bits.is_set self.completed piece) then
    self.rarity <- Histo.remove piece self.rarity

let lost_bitfield self b =
  for i = 0 to Bits.length b - 1 do
    if Bits.is_set b i then lost_have self i
  done

let lost_request t (i, ofs, len) =
  try
    let a = List.assoc i t.active in
    let n = ofs / standard_block_length in
    match a.blocks.(n) with
    | `InProgress n -> a.blocks.(n) <- `InProgress (max 0 (n-1))
    | `Done -> ()
  with
  | Not_found -> ()

let got_block t idx off s =
  if not (Bits.is_set t.completed idx) then begin
    let a = List.assoc idx t.active in
    let n = off / standard_block_length in
    match a.blocks.(n) with
    | `InProgress _ ->
      Store.write t.store (Meta.block_offset t.meta idx off) s >>= fun () ->
      a.blocks.(n) <- `Done;
      a.missing <- a.missing - 1;
      if a.missing <= 0 then begin
        Store.read t.store (Meta.piece_offset t.meta idx) a.length >>= fun s ->
        if Word160.digest_of_string s = t.meta.Meta.hashes.(idx) then begin
          Log.success "piece verified (idx=%d)" idx;
          t.active <- List.remove_assoc idx t.active;
          t.rarity <- Histo.remove_all idx t.rarity;
          t.amount_left <- Int64.(sub t.amount_left (of_int a.length));
          Bits.set t.completed idx;
          Lwt.return `Verified
          (* signal t (PieceVerified idx); *)
        end else begin
          t.active <- List.remove_assoc idx t.active;
          Log.error "piece failed hashcheck (idx=%d)" idx;
          Lwt.return `Failed
        end
      end else begin
        Lwt.return `Continue
      end
    | `Done ->
      Log.info "received a block that we already have";
      Lwt.return `Continue
  end else
    Lwt.return `Verified
  
let is_complete self =
  Bits.count self.completed = Bits.length self.completed
    
let down self =
  self.down

let up self =
  self.up

let amount_left self =
  self.amount_left

let numgot self =
  Bits.count self.completed

let have self =
  Bits.copy self.completed

let has_piece self i =
  Bits.is_set self.completed i
