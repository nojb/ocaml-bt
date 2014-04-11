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
              
let info ?exn fmt = Log.info ~sec:"download" ?exn fmt
let error ?exn fmt = Log.error ~sec:"download" ?exn fmt
let warning ?exn fmt = Log.warning ~sec:"download" ?exn fmt
let success ?exn fmt = Log.success ~sec:"download" ?exn fmt

let invalid_arg_lwt s = Lwt.fail (Invalid_argument s)

let rarest_first_cutoff = 1

type t = {
  info : Info.t;
  store : Store.t;
  scrambled : int array;
  requested : Bits.t;
  completed : Bits.t;
  mutable up : int64;
  mutable down : int64;
  mutable amount_left : int64;
  mutable rarity : Histo.t
}

let create info =
  let npieces = Array.length info.Info.hashes in
  let scrambled = Array.init npieces (fun i -> i) in
  Util.shuffle_array scrambled;
  { info; store = Store.create ();
    scrambled;
    requested = Bits.create npieces;
    completed = Bits.create npieces;
    up = 0L; down = 0L;
    amount_left = info.Info.total_length;
    rarity = Histo.empty }

let update dl =
  Lwt_list.iter_s
    (fun fi -> Store.add_file dl.store fi.Info.file_path fi.Info.file_size)
    dl.info.Info.files >>= fun () ->
  let numpieces = Array.length dl.info.Info.hashes in
  let piece_size = dl.info.Info.piece_length in
  let total_length = dl.info.Info.total_length in
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
        if Word160.digest_of_string s |> Word160.equal dl.info.Info.hashes.(i) then begin
          Bits.set dl.completed i;
          loop (Int64.(sub acc (of_int (plen i)))) (i+1)
        end else
          loop acc (i+1)
  in
  let start_time = Unix.gettimeofday () in
  loop dl.info.Info.total_length 0 >|= fun amount_left ->
  let end_time = Unix.gettimeofday () in
  success
    "torrent initialisation complete (good=%d,bad=%d,left=%Ld,secs=%.0f)"
    (Bits.count dl.completed) numpieces amount_left (end_time -. start_time);
  dl.amount_left <- amount_left
  
let got_have self piece =
  if not (Bits.is_set self.completed piece) then
    self.rarity <- Histo.add piece self.rarity

let got_bitfield self b =
  for i = 0 to Bits.length b - 1 do
    if Bits.is_set b i then got_have self i
  done
 
let lost_have self piece =
  if not (Bits.is_set self.completed piece) then
    self.rarity <- Histo.remove piece self.rarity

let lost_bitfield self b =
  for i = 0 to Bits.length b - 1 do
    if Bits.is_set b i then lost_have self i
  done

let request_lost self i =
  if not (Bits.is_set self.completed i) then
    Bits.unset self.requested i
        
(* let got_piece self i s = *)
(*   assert (String.length s = Info.piece_length self.info i); *)
(*   if Word160.digest_of_string s |> Word160.equal self.info.Info.hashes.(i) then begin *)
(*     let o = Info.piece_offset self.info i in *)
(*     Store.write self.store o s |> ignore; (\* FIXME *\) *)
(*     self.amount_left <- Int64.(sub self.amount_left (of_int (String.length s))); *)
(*     self.rarity <- H.remove_all i self.rarity; *)
(*     Bits.unset self.requested i; *)
(*     Bits.set self.completed i;  *)
(*     Trace.infof "Piece #%d verified and written to disk" i; *)
(*     true *)
(*   end *)
(*   else begin *)
(*     Bits.unset self.requested i; *)
(*     Trace.infof "Piece #%d failed hashcheck" i; *)
(*     false *)
(*   end *)

let next self have =
  let is_interesting i =
    Bits.is_set have i && not (Bits.is_set self.completed i) &&
    not (Bits.is_set self.requested i)
  in
  let r = Histo.pick is_interesting self.rarity in
  if List.length r > 0 then
    let i = List.nth r (Random.int (List.length r)) in
    Some i
  else
    None

let rec peer_ready dl p =
  info "looking a piece for peer to download (addr=%s,id=%s)"
    (Addr.to_string (Peer.addr p)) (Word160.to_hex_short (Peer.id p));
  let idx = next dl (Peer.have p) in
  let aux idx =
    Peer.request_piece p idx (Info.piece_length dl.info idx) >|= fun s ->
    if Word160.digest_of_string s |> Word160.equal dl.info.Info.hashes.(idx) then begin
      let o = Info.piece_offset dl.info idx in
      Store.write dl.store o s |> ignore; (* FIXME *)
      dl.amount_left <- Int64.(sub dl.amount_left (of_int (String.length s)));
      dl.rarity <- Histo.remove_all idx dl.rarity;
      Bits.unset dl.requested idx;
      Bits.set dl.completed idx;
      success "piece verified and written to disk (idx=%d)" idx
    end
    else begin
      Bits.unset dl.requested idx;
      error "piece failed hashcheck (idx=%d)" idx
    end;
    peer_ready dl p
  in
  match idx with
  | Some idx -> Lwt.async (fun () -> aux idx)
  | None -> ()
  
let did_request self i =
  if not (Bits.is_set self.completed i) then
    Bits.set self.requested i

  (* (\* FIXME end-game *\) *)
  (* if Bits.count interesting > 0 then *)
  (*   let r = H.pick (Bits.is_set have) self.rarity in *)
  (*   if List.length r > 0 then *)
  (* let rec loop i = *)
  (*   if i >= Bits.length self.requested then None *)
  (*   else *)
  (*   if Bits.is_set self.requested i && have i then Some i *)
  (*   else loop (i+1) *)
  (* in *)
  (* match loop 0 with *)
  (* | Some i -> *)
  (*   Trace.infof "Picker: next: found %d" i; *)
  (*   Some i *)
  (* | None -> *)
  (*   Trace.infof "Picker: did not find in the requested set"; *)
  (*   if Bits.count self.completed < rarest_first_cutoff then *)
  (*     let rec loop i = *)
  (*       if i >= Bits.length self.completed then None *)
  (*       else if have self.scrambled.(i) then Some i *)
  (*       else loop (i+1) *)
  (*     in *)
  (*     loop 0 *)
  (*   else *)
  (*     let r = H.pick have self.rarity in *)
  (*     if List.length r > 0 then *)
  (*       Some (Random.int (List.length r) |> List.nth r) *)
  (*     else *)
  (*       None *)

let is_complete self =
  Bits.count self.completed = Bits.length self.completed
    
(* let get_block self i o l = *)
(*   if i < 0 || i >= Array.length self.hashes || o < 0 || *)
(*      l < 0 || o + l > Info.piece_length self.info i then *)
(*     invalid_arg_lwt "Torrent.get_block" *)
(*   else *)
(*     if Bits.is_set self.completed i then *)
(*       let o = Int64.add (Info.piece_offset self.info i) (Int64.of_int o) in *)
(*       Store.read self.store o l *)
(*     else *)
(*       invalid_arg_lwt "Torrent.get_block" *)


(* match self.pieces.(i) with *)
(* | DONE -> *)
    (*   let o = Int64.(mul (of_int i) (of_int self.piece_size) |> add (of_int o)) in *)
    (*   Store.read self.store o l *)
    (* | _ -> *)
    (*   invalid_arg_lwt "Torrent.get_block" *)

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
