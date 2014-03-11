module H = Histo.Make (Histo.Int) (Histo.Int)
    
type t = {
  numpieces : int;
  requested : Bits.t;
  completed : Bits.t;
  scrambled : int array;
  mutable numgot : int;
  mutable rarity : H.t
}

let shuffle_array a =
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i+1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done

let rarest_first_cutoff = 1

let create numpieces =
  let self = { numpieces; requested = Bits.create numpieces;
    scrambled = Array.init numpieces (fun i -> i);
    rarity = H.empty; numgot = 0; completed = Bits.create numpieces }
  in
  shuffle_array self.scrambled;
  self

let got_have self piece =
  if not (Bits.is_set self.completed piece) then
    self.rarity <- H.add piece self.rarity
 
let lost_have self piece =
  if not (Bits.is_set self.completed piece) then
    self.rarity <- H.remove piece self.rarity

(* let did_request self piece = *)
(*   Bits.set self.requested piece *)

let got_piece self piece =
  self.rarity <- H.remove_all piece self.rarity;
  self.numgot <- self.numgot + 1;
  Bits.unset self.requested piece;
  Bits.set self.completed piece

let next self have =
  let rec loop i =
    if i >= Bits.length self.requested then None
    else
    if Bits.is_set self.requested i && have i then Some i
    else loop (i+1)
  in
  match loop 0 with
  | Some i ->
    Trace.infof "Picker: next: found %d" i;
    Some i
  | None ->
    Trace.infof "Picker: did not find in the requested set";
    (* let r = H.pick (fun i -> Bits.is_set self.requested i && have i) self.rarity in *)
    (* if List.length r > 0 then *)
    (*   Some (Random.int (List.length r) |> List.nth r) *)
    (* else *)
    if self.numgot < rarest_first_cutoff then
      let rec loop i =
        if i >= self.numpieces then None
        else if have self.scrambled.(i) then Some i
        else loop (i+1)
      in
      loop 0
    else
      let r = H.pick have self.rarity in
      if List.length r > 0 then
        Some (Random.int (List.length r) |> List.nth r)
      else
        None

let is_complete self =
  self.numgot = self.numpieces
