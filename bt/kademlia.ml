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

module MakeTable (Key : KEY) = struct
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

module Table = MakeTable
    (struct
      type t = SHA1.t
      let length = 160
      let nth key i = Bitstring.get (SHA1.to_bin key, 0, 160) i <> 0
      let equal x y = x = y
    end)

type status =
  | Good
  | Bad
  | Unknown
  | Pinged

let string_of_status = function
  | Good -> "good"
  | Bad -> "bad"
  | Unknown -> "unknown"
  | Pinged -> "pinged"

type node = {
  id : SHA1.t;
  addr : Addr.t;
  mutable last_changed : float;
  mutable status : status;
}

let string_of_node n =
  Printf.sprintf "%s at %s was %s %d sec ago"
    (SHA1.to_hex_short n.id)
    (Addr.to_string n.addr)
    (string_of_status n.status)
    (truncate (Unix.time () -. n.last_changed))
  
type t = {
  mutable tbl : node Table.t;
  mutable tbl_last_change : float
}

let create () =
  { tbl = Table.empty; tbl_last_change = Unix.time () }

let make_node id addr status =
  { id; addr; status; last_changed = Unix.time () }

let mark node st =
  node.last_changed <- Unix.time ();
  node.status <- st

let touch node =
  node.last_changed <- Unix.time ()

let update ping table st id data =
  assert false

let find_node table id =
  List.map (fun (_, n) -> n.id, n.addr) (Table.find id table.tbl)
