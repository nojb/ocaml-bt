let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type active_block = {
  a_piece : int;
  a_block : int;
  a_peer : Peer.t;
  a_sent_at : float
}

type pending_piece = {
  p_index : int;
  mutable p_reqs : int;
  p_salt : int;
  mutable p_complete : bool
}

type t = {
  meta : Metadata.t;
  tor : Torrent.t;
  rarity : int array;
  pending : pending_piece array;
  mutable active : active_block list
}

let compare_by_weight r p1 p2 =
  let w p =
    let m = Torrent.missing_blocks_in_piece r.tor p.p_index in
    let reqs = p.p_reqs in
    let a = if m > reqs then m - reqs else Metadata.block_count r.meta p.p_index + reqs in
    (a, r.rarity.(p.p_index))
  in
  match p1.p_complete, p2.p_complete with
  | true, true -> 0
  | true, false -> 1
  | false, true -> -1
  | false, false ->
    let (a1, h1) = w p1 in
    let (a2, h2) = w p2 in
    if a2 < a1 then 1 else
    if a1 < a2 then -1 else
    if h1 < h2 then -1 else
    if h2 < h1 then 1 else
    if p1.p_salt < p2.p_salt then 1 else
    if p2.p_salt < p1.p_salt then -1
    else 0

let get_next_requests t peer n =
  Array.sort (compare_by_weight t) t.pending;
  let rec loop acc i =
    if i >= Array.length t.pending || List.length acc >= n then List.rev acc
    else
      let p = t.pending.(i) in
      if p.p_complete then List.rev acc
      else 
      if not (Peer.has_piece peer p.p_index) then loop acc (i+1)
      else
        let rec loop' acc j =
          if List.length acc >= n then List.rev acc else
          if j >= Metadata.block_count t.meta p.p_index then loop acc (i+1) else
          if List.exists (fun r -> r.a_piece = p.p_index && r.a_block = j) t.active then loop' acc (j+1) else
          if not (Torrent.has_block t.tor p.p_index j) then loop' ((p, j) :: acc) (j+1)
          else loop' acc (j+1)
        in
        loop' acc 0
  in
  let reqs = loop [] 0 in
  List.iter (fun (p, j) ->
      t.active <- {a_piece = p.p_index; a_block = j; a_peer = peer; a_sent_at = Unix.time ()} :: t.active;
      p.p_reqs <- p.p_reqs + 1) reqs;
  List.map (fun (p, j) -> p.p_index, j) reqs

let lookup t i =
  let rec loop j =
    if j >= Array.length t.pending then raise Not_found
    else if t.pending.(j).p_index = i then t.pending.(j) else loop (j+1)
  in
  loop 0

let decrease_request_count t i =
  let p = lookup t i in
  p.p_reqs <- p.p_reqs - 1

let request_ttl_secs = 90

let refill_upkeep_period_msec = 10.0

let rec upkeep_pulse t =
  let now = Unix.time () in
  let too_old = now -. float request_ttl_secs in
  let old, keep = List.partition (fun r -> r.a_sent_at <= too_old) t.active in
  t.active <- keep;
  List.iter (fun r ->
      Log.debug "cancelling %d(%d/%d) because request is too old"
        r.a_piece r.a_block (Metadata.block_count t.meta r.a_piece);
      Peer.send_cancel r.a_peer (r.a_piece, r.a_block);
      decrease_request_count t r.a_piece) old;
  Lwt_unix.sleep refill_upkeep_period_msec >>= fun () ->
  upkeep_pulse t

let peer_declined_all_requests tor peer =
  List.iter (fun r -> if r.a_peer == peer then decrease_request_count tor r.a_piece) tor.active;
  tor.active <- List.filter (fun r -> r.a_peer != peer) tor.active

let got_block tor peer i b =
  List.iter begin fun r ->
    if r.a_piece = i && r.a_block = b then begin
      decrease_request_count tor i;
      if r.a_peer != peer then Peer.send_cancel r.a_peer (i, b)
    end
  end tor.active;
  tor.active <- List.filter (fun r -> r.a_piece <> i || r.a_block <> b) tor.active

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

let got_piece r i =
  let p = lookup r i in
  p.p_complete <- true

let got_bad_piece r i =
  (* FIXME *)
  ()
  
let create m t =
  let numpieces = Metadata.piece_count m in
  let create_pending_piece i =
    { p_index = i; p_reqs = 0; p_salt = Random.bits (); p_complete = Torrent.has_piece t i }
  in
  let r =
    { meta = m; tor = t;
      pending = Array.init numpieces create_pending_piece; active = [];
      rarity = Array.create numpieces 0 }
  in
  Lwt.async (fun () -> upkeep_pulse r);
  r
