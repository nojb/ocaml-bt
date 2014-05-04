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

type active_block = {
  a_piece : int;
  a_block : int;
  a_peer : Peer.t;
  a_sent_at : float
}

type pending_piece = {
  p_index : int;
  mutable p_reqs : int;
  p_salt : int
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

let compare_by_weight t p1 p2 =
  match p1, p2 with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some p1, Some p2 ->
    let w p =
      let m = Bits.missing t.completed.(p.p_index) in
      let r = p.p_reqs in
      let a = if m > r then m - r else Metadata.block_count t.meta p.p_index + r in
      (a, t.rarity.(p.p_index), p.p_salt)
    in
    let (a1, h1, s1) = w p1 in
    let (a2, h2, s2) = w p2 in
    if a2 < a1 then 1 else
    if a1 < a2 then -1 else
    if h1 < h2 then -1 else
    if h2 < h1 then 1 else
    if s1 < s2 then 1 else
    if s2 < s1 then -1
    else 0
  
let get_next_requests t peer n =
  Array.sort (compare_by_weight t) t.pending;
  (* for i = 0 to (Array.length t.pending) - 2 do *)
    (* assert (compare_by_weight t t.pending.(i) t.pending.(i+1) <= 0) *)
  (* done; *)
  let rec loop acc i =
    if i >= Array.length t.pending || List.length acc >= n then List.rev acc
    else match t.pending.(i) with
      | None -> List.rev acc
      | Some p ->
        if not (Peer.has_piece peer p.p_index) then loop acc (i+1)
        else
          let rec loop' acc j =
            if List.length acc >= n then List.rev acc else
            if j >= Bits.length t.completed.(p.p_index) then loop acc (i+1) else
            if List.exists (fun r -> r.a_piece = p.p_index && r.a_block = j) t.active then loop' acc (j+1) else
            if not (Bits.is_set t.completed.(p.p_index) j) then loop' ((p, j) :: acc) (j+1)
            else loop' acc (j+1)
          in
          loop' acc 0
  in
  let reqs = loop [] 0 in
  List.iter (fun (p, j) ->
      t.active <- {a_piece = p.p_index; a_block = j; a_peer = peer; a_sent_at = Unix.time ()} :: t.active;
      p.p_reqs <- p.p_reqs + 1) reqs;
  List.map (fun (p, j) -> Metadata.block t.meta p.p_index j) reqs

let decrease_request_count t i =
  match t.pending.(i) with
  | Some p -> p.p_reqs <- p.p_reqs - 1
  | None -> ()

(* let request_ttl_secs = 90 *)

(* let rec upkeep_pulse t = *)
(*   let now = Unix.time () in *)
(*   let too_old = now -. float request_ttl_secs in  *)
(*   let old, keep = List.partition (fun r -> r.a_sent_at <= too_old) t.active in *)
(*   t.active <- keep; *)
(*   List.iter (fun r -> *)
(*       Peer.send_cancel r.a_peer r.a_piece r.a_block; *)
(*       decr_request_count t r.a_piece) old; *)
(*   Lwt_unix.sleep refill_upkeep_period_msec >>= fun () -> upkeep_pulse t *)

let create meta =
  let numpieces = Metadata.piece_count meta in
  let create_pending_piece i = { p_index = i; p_reqs = 0; p_salt = Random.bits () } in
  let dl = {
    meta; store = Store.create ();
    active = [];
    pending = Array.init numpieces (fun i -> Some (create_pending_piece i));
    completed = Array.init numpieces (fun i -> Bits.create (Metadata.block_count meta i));
    up = 0L; down = 0L;
    amount_left = Metadata.total_length meta;
    rarity = Array.create numpieces 0
  } in
  Metadata.iter_files meta
    (fun fi -> Store.add_file dl.store fi.Metadata.file_path fi.Metadata.file_size) >>= fun () ->
  let plen = Metadata.piece_length dl.meta in
  let rec loop good acc i =
    if i >= numpieces then
      Lwt.return (good, acc)
    else
      let off = Metadata.piece_offset dl.meta i in
      Store.read dl.store off (plen i) >>= fun s ->
      if SHA1.digest_of_string s |> SHA1.equal (Metadata.hash dl.meta i) then begin
        Bits.set_all dl.completed.(i);
        dl.pending.(i) <- None;
        loop (good+1) (Int64.(sub acc (of_int (plen i)))) (i+1)
      end else
        loop good acc (i+1)
  in
  let start_time = Unix.gettimeofday () in
  loop 0 (Metadata.total_length dl.meta) 0 >>= fun (good, amount_left) ->
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
  self.rarity.(piece) <- self.rarity.(piece) + 1

let got_bitfield self b =
  for i = 0 to Bits.length b - 1 do
    if Bits.is_set b i then got_have self i
  done
 
let lost_have self piece =
  self.rarity.(piece) <- self.rarity.(piece) - 1

let lost_bitfield self b =
  for i = 0 to Bits.length b - 1 do
    if Bits.is_set b i then lost_have self i
  done

let lost_request t (i, ofs, len) = (* FIXME which peer cancelled ? *)
  let b = Metadata.block_number t.meta ofs in
  (* let rec loop = function *)
  (*   | [] -> () *)
  (*   | a :: ax -> *)
  (*     if a.a_piece = i && a.a_block = b then begin *)
  (*       decr_request_count t i; *)
  (*       ax *)
  (*     end *)
  (*     else *)
  (*       a :: loop ax *)
  (* in *)
  (* t.active <- loop t.active *)
  t.active <- List.filter (fun a -> a.a_piece <> i || a.a_block <> b) t.active;
  decrease_request_count t i

let got_block t idx off s =
  (* t.down <- Int64.add t.down (Int64.of_int (String.length s)); *)
  (* if not (Bits.has_all t.completed.(idx)) then begin *)
  let b = Metadata.block_number t.meta off in
  if not (Bits.is_set t.completed.(idx) b) then begin
    Store.write t.store (Metadata.block_offset t.meta idx off) s >>= fun () ->
    Bits.set t.completed.(idx) b;
    Log.debug "got block %d:%d (%d remaining)" idx b
      (Bits.length t.completed.(idx) - Bits.count t.completed.(idx));
    t.active <- List.filter (fun r -> r.a_piece <> idx || r.a_block <> b) t.active;
    if Bits.has_all t.completed.(idx) then begin
      Store.read t.store (Metadata.piece_offset t.meta idx)
        (Metadata.piece_length t.meta idx) >>= fun s ->
      if SHA1.digest_of_string s = Metadata.hash t.meta idx then begin
        Log.success "piece verified (idx=%d)" idx;
        t.pending.(idx) <- None;
        (* t.amount_left <- Int64.(sub t.amount_left (of_int (Metadata.piece_length t.meta idx))); *)
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
    Lwt.return (if Bits.has_all t.completed.(idx) then `Verified else `Continue)
  end
  (* end else *)
    (* Lwt.return `Verified *)
  
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

let set_rarity self a =
  assert (Array.length a = Array.length self.rarity);
  Array.blit a 0 self.rarity 0 (Array.length a)
