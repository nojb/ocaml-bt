let kilobytes n = n * 1024
let block_size = kilobytes 16

module S2 = Set.Make (struct type t = int * int let compare = compare end)
module S3 = Set.Make (struct type t = int * int * int let compare = compare end)

type piece_progress =
  { pcblocks : int;
    mutable pchave : S2.t;
    mutable pcpend : S2.t;
    buffer : string }

type piece_status =
  | PENDING
  | DONE
  | PROGRESS of piece_progress      
    
type t = {
  total_length : int64;
  numpieces : int;
  piece_size : int;
  mutable downloading : S3.t;
  pieces : piece_status array;
  store : Store.t;
  hashes : Word160.t array;
  mutable up : int64;
  mutable down : int64;
  mutable amount_left : int64
}

let numgot pieces =
  let rec loop acc i =
    if i >= Array.length pieces then acc
    else
      match pieces.(i) with
      | DONE -> loop (acc+1) (i+1)
      | _ -> loop acc (i+1)
  in
  loop 0 0

let safe_to_int n =
  let m = Int64.to_int n in
  assert (Int64.(compare (of_int m) n) = 0);
  m

let create info =
  let (>|=) = Lwt.(>|=) in
  let (>>=) = Lwt.(>>=) in
  Store.create info.Info.files >>= fun store ->
  let numpieces = Array.length info.Info.pieces in
  let hashes = Array.map (fun pi -> pi.Info.piece_digest) info.Info.pieces in
  let pieces = Array.map (fun _ -> PENDING) info.Info.pieces in
  let piece_size = info.Info.piece_length in
  let total_length = info.Info.total_length in
  let plen i =
    if i < numpieces - 1 then piece_size
    else
      Int64.(mul (of_int i) (of_int piece_size) |>
             sub total_length |> safe_to_int)
  in
  let rec loop i =
    if i >= numpieces then Lwt.return_unit
    else
      let o = Int64.(mul (of_int i) (of_int piece_size)) in
      Store.read store o (plen i) >>= begin fun s ->
        if Word160.digest_of_string s |> Word160.equal hashes.(i) then
          pieces.(i) <- DONE;
        loop (i+1)
      end
  in
  loop 0 >|= fun () ->
  let rec loop acc i =
    if i >= numpieces then acc
    else
      loop (Int64.(sub acc (of_int (plen i)))) (i+1)
  in
  let amount_left = loop info.Info.total_length 0 in
  Trace.infof "Torrent initialisation complete. Have %d/%d pieces (%Ld bytes remaining)"
    (numgot pieces) numpieces amount_left;
  { total_length;
    numpieces;
    piece_size;
    downloading = S3.empty;
    pieces;
    store;
    hashes;
    up = 0L;
    down = 0L;
    amount_left }

let have self =
  Bits.to_bin self

let _piecelen self i =
  if i < Array.length self.hashes - 1 then
    self.piece_size
  else
    Int64.(mul (of_int i) (of_int self.piece_size) |>
           sub self.total_length |> safe_to_int)

let blocks_of_piece self i : S2.t =
  let l = _piecelen self i in
  let rec loop s x =
    if x+block_size >= l then S2.add (x, l-x) s
    else loop (S2.add (x, block_size) s) (x+block_size)
  in
  loop S2.empty 0

let new_request self i =
  let choose p =
    if S2.is_empty p.pcpend then
      None
    else begin
      let (o, l) = S2.choose p.pcpend in
      p.pcpend <- S2.remove (o, l) p.pcpend;
      self.downloading <- S3.add (i, o, l) self.downloading;
      Some (o, l)
    end
  in
  match self.pieces.(i) with
  | PENDING ->
    let blks = blocks_of_piece self i in
    let p =
      { pcblocks = S2.cardinal blks;
        pchave = S2.empty;
        pcpend = blks;
        buffer = String.create (_piecelen self i) } in
    self.pieces.(i) <- PROGRESS p;
    choose p
  | PROGRESS p ->
    choose p
  | DONE ->
    None
        
let request_lost self i o l =
  (* assert (S.mem (i, off, len) self.downloading); *)
  self.downloading <- S3.remove (i, o, l) self.downloading;
  match self.pieces.(i) with
  | DONE ->
    Trace.infof "request_lost: piece is DONE: %d %d %d" i o l
  | PENDING ->
    Trace.infof "request_lost: piece is PENDING: %d %d %d" i o l
  | PROGRESS p ->
    p.pcpend <- S2.add (o, l) p.pcpend

let got_block self i o s =
  self.downloading <- S3.remove (i, o, String.length s) self.downloading;
  match self.pieces.(i) with
  | DONE ->
    Trace.infof "got_block: piece is DONE: %d %d %d" i o (String.length s);
    false
  | PENDING ->
    Trace.infof "got_block: piece is PENDING: %d %d %d" i o (String.length s);
    false
  | PROGRESS p ->
    String.blit s 0 p.buffer 0 (String.length s);
    p.pchave <- S2.add (o, String.length s) p.pchave;
    if p.pcblocks = S2.cardinal p.pchave then
      if Word160.digest_of_string s |> Word160.equal self.hashes.(i) then begin
        let o = Int64.(mul (of_int self.piece_size) (of_int i)) in
        Store.write self.store o p.buffer;
        self.pieces.(i) <- DONE;
        self.amount_left <- Int64.(sub self.amount_left (of_int (_piecelen self i)));
        Trace.infof "Piece #%d verified and written to disk" i;
        true
      end
      else begin
        p.pchave <- S2.empty;
        p.pcpend <- blocks_of_piece self i;
        false
      end
    else
      true

let available_requests self i =
  match self.pieces.(i) with
  | PENDING -> true
  | DONE -> false
  | PROGRESS p -> not (S2.is_empty p.pcpend)

let is_complete self =
  let rec loop i =
    if i >= Array.length self.pieces then true
    else
      match self.pieces.(i) with
      | DONE -> loop (i+1)
      | _ -> false
  in
  loop 0
    
let get_block self i o l =
  if i < 0 || i >= Array.length self.hashes || o < 0 ||
     l < 0 || o + l > _piecelen self i then
    Lwt.return_none
  else
    let (>|=) = Lwt.(>|=) in
    match self.pieces.(i) with
    | DONE ->
      let o = Int64.(mul (of_int i) (of_int self.piece_size) |> add (of_int o)) in
      Store.read self.store o l >|= fun s -> Some s
    | _ ->
      Lwt.return_none
