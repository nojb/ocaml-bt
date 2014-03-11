type 'a t =
  ('a, unit) Hashtbl.t

let create ?random n =
  Hashtbl.create ?random n

let clear hs =
  Hashtbl.clear hs

let reset hs =
  Hashtbl.reset hs

let copy hs =
  Hashtbl.copy hs

let add hs x =
  Hashtbl.replace hs x ()

let mem hs x =
  Hashtbl.mem hs x

let remove hs x =
  Hashtbl.remove hs x

let iter f hs =
  Hashtbl.iter (fun x () -> f x) hs

let fold f hs a =
  Hashtbl.fold (fun x () a -> f x a) hs a

let cardinal hs =
  Hashtbl.length hs

let choose h =
  let r = ref None in
  try
    Hashtbl.iter (fun x () -> r := Some x; raise Exit) h;
    raise Not_found
  with
  | Exit ->
    match !r with
    | None -> assert false
    | Some x -> x
