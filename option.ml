type 'a t = 'a option
let bind o f =
  match o with
  | None -> None
  | Some x -> f x
let return x = Some x
let fail _ = None
let either f1 f2 =
  fun x ->
    match f1 x with
    | None -> f2 x
    | Some _ as o -> o
let rec map (f : 'a -> 'b option) (l : 'a list) : 'b list option =
  match l with
  | [] -> Some []
  | x :: xs ->
    match f x with
    | None -> None
    | Some fx ->
      match map f xs with
      | None -> None
      | Some fxs -> Some (fx :: fxs)
let rec fold f acc = function
  | [] -> Some acc
  | x :: xs ->
    match f acc x with
    | None -> None
    | Some fx -> fold f fx xs
let (>>=) o f = bind o f
let (>|=) o f = bind o (fun x -> Some (f x))
