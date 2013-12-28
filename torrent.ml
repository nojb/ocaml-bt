module H = Histo.Make (Histo.Int) (Histo.Int)
    
type t = {
  mutable downloaded : int64;
  total_length : int64;
  piece_size : int;
  started : Bits.t;
  available : Bits.t;
  completed : Bits.t;
  scrambled : int array;
  mutable rarity : H.t;
  store : Store.t;
  hashes : Word160.t array;
  piecelen : int array;
  pieceoff : int64 array;
  mutable amount_left : int64
}

let shuffle a =
  assert false

let rarest_first_cutoff = 1

let create info =
  assert false
  (* { numpieces; started = Bits.t } *)

let got_have self piece =
  if not (Bits.is_set self.completed piece) then
    self.rarity <- H.add piece self.rarity;
  Bits.set self.available piece
 
let lost_have self piece =
  if not (Bits.is_set self.completed piece) then
    self.rarity <- H.remove piece self.rarity;
  if not (H.mem piece self.rarity) then
    Bits.unset self.available piece

let request_lost self i =
  Bits.unset self.started i

let request_sent self piece =
  Bits.set self.started piece

let completed self =
  Bits.copy self.completed
    
let got_piece self piece s =
  self.downloaded <- Int64.(add self.downloaded (of_int self.piecelen.(piece)));
  Bits.unset self.started piece;
  if Word160.digest_of_string s |> Word160.equal self.hashes.(piece) then begin
    Store.write self.store self.pieceoff.(piece) s;
    Bits.set self.completed piece;
    self.amount_left <- Int64.(sub self.amount_left (of_int self.piecelen.(piece)));
    (* FIXME check if done *)
    true
  end else
    (* invalid *)
    false

let next_piece self havefunc =
  if Bits.count self.completed < rarest_first_cutoff then
    let rec loop i =
      if havefunc self.scrambled.(i) then
        let piece = self.scrambled.(i) in
        Some (piece, self.piecelen.(piece))
      else
        loop (i+1)
    in
    loop 0
  else
    let rarest = H.pick havefunc self.rarity in
    if List.length rarest > 0 then
      let piece = List.nth rarest (Random.int (List.length rarest)) in
      Some (piece, self.piecelen.(piece))
    else
      None

let is_complete self =
  Bits.count self.completed = Bits.length self.completed

let completion self =
  float (Bits.count self.completed) /. float (Bits.length self.completed) *. 100.0

let (>|=) = Lwt.(>|=)

let get_block self i off len =
  if Bits.is_set self.completed i then
    Store.read self.store Int64.(add self.pieceoff.(i) (of_int off)) len >|= fun s -> Some s
  else
    Lwt.return_none
