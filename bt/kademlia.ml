module type KEY = sig
  type t
  val length : int
  val nth : t -> int -> bool
  val equal : t -> t -> bool
end

module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : ?max:int -> ?pred:('a -> bool) -> key -> 'a t -> (key * 'a) list
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

module Make (Key : KEY) = struct
  type key = Key.t
  type 'a t =
    | Empty
    | NodeE of 'a t * 'a t
    | NodeF of Key.t * 'a

  let empty = Empty

  let branch_out k1 n1 k2 n2 i =
    let rec loop i =
      match Key.nth k1 i, Key.nth k2 i with
      | true, false -> NodeE (NodeF (k2, n2), NodeF (k1, n1))
      | false, true -> NodeE (NodeF (k1, n1), NodeF (k2, n2))
      | true, true -> NodeE (Empty, loop (i+1))
      | false, false -> NodeE (loop (i+1), Empty)
    in
    loop i

  let add k d tree =
    let rec loop i = function
      | Empty
      | NodeE (Empty, Empty) ->
        NodeF (k, d)
      | NodeE (l, r) ->
        if Key.nth k i then NodeE (l, loop (i+1) r) else NodeE (loop (i+1) l, r)
      | NodeF (u, x) ->
        if Key.equal k u then NodeF (k, d) else branch_out k d u x i
    in
    loop 0 tree

  let find ?(max = 8) ?(pred = (fun _ -> true)) k tree =
    let rec loop i lst tree =
      if List.length lst >= max then lst
      else match tree with
        | Empty ->
          lst
        | NodeF (k, x) ->
          if pred x then (k, x) :: lst else lst
        | NodeE (l, r) ->
          if Key.nth k i then loop (i+1) (loop (i+1) lst r) l
          else loop (i+1) (loop (i+1) lst l) r
    in
    loop 0 [] tree
  
  let rec iter f = function
    | Empty -> ()
    | NodeF (k, d) -> f k d
    | NodeE (l, r) -> iter f l; iter f r
end

(* module K = struct *)
(*   type t = int array *)
(*   type key = t *)
(*   let length = 4 *)
(*   let create a0 = *)
(*     let a = Array.create length false in *)
(*     Array.blit a0 0 a 0 (min (Array.length a0) length); *)
(*     a *)
(*   let to_string a = *)
(*     let str = String.create (Array.length a) in *)
(*     for i = 0 to Array.length a - 1 do *)
(*       str.[i] <- if a.(i) <> 0 then '1' else '0' *)
(*     done; *)
(*     str *)
(*   let printer fmt a = *)
(*     Format.fprintf fmt "%s" (to_string a) *)
(*   let nth a i = a.(i) <> 0 *)
(*   let equal k1 k2 = *)
(*     let rec loop i = *)
(*       if i >= length then true *)
(*       else if (k1.(i) = 0 && k2.(i) <> 0) || (k1.(i) <> 0 && k2.(i) = 0) then false *)
(*       else loop (i+1) *)
(*     in *)
(*     loop 0 *)
(* end *)

(* include Make (K) *)

include Make
    (struct
      type t = SHA1.t
      let length = 160
      let nth key i = Bitstring.get (SHA1.to_bin key, 0, 160) i <> 0
      let equal x y = x = y
    end)
