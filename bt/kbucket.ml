module List = struct
  include List
  let last n l =
    let rec loop n = function
      | rem when n = 0 -> rem
      | _ :: rem -> loop (n - 1) rem
      | [] -> invalid_arg "List.last"
    in
    loop (max 0 (List.length l - n)) l
  let rec first n l =
    match n, l with
    | 0, _ -> []
    | n, [] -> invalid_arg "List.first"
    | n, x :: rem -> x :: first (n - 1) rem
end

module type KEY = sig
  type t
  val nth : t -> int -> bool
  val length : t -> int
  val equal : t -> t -> bool
  val distance : t -> t -> Z.t
  val pp : Format.formatter -> t -> unit
end

module type DATA = sig
  type t
  val equal : t -> t -> bool
  val merge : t -> t -> t
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type key
  type data
  type t
  val empty : ?k:int -> ?p:int -> key -> t
  val length : t -> int
  val add : key -> data -> t -> [ `Ok of t | `Ping of (key * data) list ]
  val get : key -> t -> data option
  val remove : key -> t -> t
  val closest : key -> int -> t -> (key * data) list
  val to_list : t -> (key * data) list
  val pp : Format.formatter -> t -> unit
end

module Make (K : KEY) (D : DATA) = struct
  type key = K.t
  type data = D.t

  type node =
    | Inner of node * node
    | Leaf of bool * (key * data) list

  type t =
    { root : node;
      id : key;
      k : int; (* nodes per backet *)
      p : int (* nodes to ping *) }

  exception Ping of (key * data) list

  let rec remove_assoc k = function
    | [] -> []
    | (k', _) :: rem when K.equal k k' -> rem
    | _ :: rem -> remove_assoc k rem

  let add k d t =
    let rec split i bucket =
      let highs, lows = List.partition (fun (k, _) -> K.nth k i) bucket in
      Format.eprintf "Splitting at %d@." i;
      let lo = Leaf (not (K.nth t.id i), lows) and hi = Leaf (K.nth t.id i, highs) in
      Inner (lo, hi)
    and insert k d i = function
      | Inner (lo, hi) ->
          if K.nth k i then
            Inner (lo, insert k d (i+1) hi)
          else
            Inner (insert k d (i+1) lo, hi)
      | Leaf (cansplit, bucket) as node ->
          let rec loop = function
            | [] when List.length bucket < t.k ->
                Leaf (cansplit, (k, d) :: bucket)
            | [] when cansplit ->
                insert k d i (split i bucket)
            | [] ->
                raise (Ping (List.last t.p bucket))
            | (k', d') :: _ when K.equal k k' ->
                let d'' = D.merge d d' in
                if D.equal d'' d' && not (D.equal d d') then node
                else Leaf (cansplit, (k', d'') :: remove_assoc k' bucket)
            | _ :: rem -> loop rem
          in
          loop bucket
    in
    try `Ok { t with root = insert k d 0 t.root } with Ping l -> `Ping l

  let get k t =
    let rec fetch i = function
      | Inner (lo, hi) ->
          fetch (i+1) (if K.nth k i then hi else lo)
      | Leaf (_, bucket) ->
          let _, d = List.find (fun (k', _) -> K.equal k k') bucket in
          d
    in
    try Some (fetch 0 t.root) with Not_found -> None

  let remove k t =
    let rec rm i = function
      | Inner (lo, hi) ->
          if K.nth k i then Inner (lo, rm (i+1) hi) else Inner (rm (i+1) lo, hi)
      | Leaf (cansplit, bucket) ->
          Leaf (cansplit, remove_assoc k bucket)
    in
    { t with root = rm 0 t.root }

  let empty ?(k = 20) ?(p = 3) id =
    { root = Leaf (true, []); id; k; p }

  let length t =
    let rec aux acc = function
      | Inner (lo, hi) -> aux (aux acc lo) hi
      | Leaf (_, bucket) -> acc + List.length bucket
    in
    aux 0 t.root

  let closest k n t =
    let rec find n i = function
      | Inner (lo, hi) ->
          let first, second = if K.nth k i then hi, lo else lo, hi in
          let i = i+1 in
          let x = find n i first in
          if List.length x < n then x @ find (n - List.length x) i second else x
      | Leaf (_, bucket) ->
          let cmp (k, _) (k', _) = Z.compare (K.distance k t.id) (K.distance k' t.id) in
          let bucket = List.sort cmp bucket in
          List.first (min n (List.length bucket)) bucket
    in
    find n 0 t.root

  let to_list t =
    let rec loop n k =
      match n with
      | Inner (lo, hi) ->
          loop lo (loop hi k)
      | Leaf (_, bucket) ->
          bucket @ k
    in
    loop t.root []

  let pp ppf t =
    let aux ppf =
      List.iter
        (fun (k, d) -> Format.fprintf ppf "@ @[<hov 2>(%a@ %a)@]" K.pp k D.pp d)
    in
    let rec pn ppf = function
      | Inner (lo, hi) ->
          Format.fprintf ppf "@[<hv 2>(inner@ %a@ %a)@]" pn lo pn hi
      | Leaf (true, bucket) ->
          Format.fprintf ppf "@[<hv 2>(split%a)@]" aux bucket
      | Leaf (false, bucket) ->
          Format.fprintf ppf "@[<hv 2>(non-split%a)@]" aux bucket
    in
    Format.fprintf ppf "@[<v 2>(%a@ %a)@]" K.pp t.id pn t.root

end

module SHA1 = struct
  type t = string
  let length s = String.length s
  let nth s i =
    if i < 0 || i >= 8 * String.length s then invalid_arg "nth";
    let j = i / 8 and k = i mod 8 in
    Char.code s.[j] land (1 lsl (7-k)) <> 0
  let equal s1 s2 = s1 = s2
  let distance s1 s2 =
    let to_z s =
      let rec loop n i =
        if i >= String.length s then n
        else
          let c = Char.code s.[i] in
          loop Z.(~$c + ~$256 * n) (i+1)
      in
      loop Z.zero 0
    in
    Z.logxor (to_z s1) (to_z s2)
  let pp ppf s =
    for i = 0 to String.length s - 1 do
      let rec loop n i =
        if i < 8 then begin
          Format.fprintf ppf "%c" (if n land 0x80 <> 0 then '1' else '0');
          loop (n lsl 1) (i + 1)
        end
      in
      loop (Char.code s.[i]) 0
      (* Format.fprintf ppf *)
      (* Format.fprintf ppf "%02X" (Char.code s.[i]) *)
    done
end

module T = Make (SHA1) (struct type t = int let equal n m = n = m let merge n m = m let pp ppf = Format.fprintf ppf "%d" end)
