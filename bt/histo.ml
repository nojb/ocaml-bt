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

module type PRIO = sig
  type t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val compare : t -> t -> int
end

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type prio
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : key -> t -> bool
  val singleton : key -> t
  val add : key -> t -> t
  val find : key -> t -> prio
  val min_elt : t -> key
  val remove_min : t -> t
  val remove : key -> t -> t
  val remove_all : key -> t -> t
  val of_list : key list -> t
  val to_list : t -> (key * prio) list
  val pick : (key -> bool) -> t -> key list
end

module Make (O : ORD) (P : PRIO) : S with type key = O.t and type prio = P.t
  = struct
  module T = struct
    type t = O.t * P.t
    let compare (x, y) (a, b) =
      let c = P.compare y b in
      if c = 0 then O.compare x a else c
  end
  module S = Set.Make (T)
  module M = Map.Make (O)
  type key = O.t
  type prio = P.t
  type t = S.t * P.t M.t
  let empty =
    S.empty, M.empty
  let is_empty (s, _) =
    S.is_empty s
  let mem k (_, m) =
    M.mem k m
  let singleton k =
    S.singleton (k, P.one), M.singleton k P.one
  let add k (s, m) =
    if M.mem k m then
      let p = M.find k m in
      let s = S.remove (k, p) s in
      let p = P.succ p in
      S.add (k, p) s, M.add k p m
    else
      S.add (k, P.one) s, M.add k P.one m
  let find k (_, m) =
    M.find k m
  let min_elt (s, _) =
    let k, _ = S.min_elt s in
    k
  let remove_min (s, m) =
    let k, p = S.min_elt s in
    S.remove (k, p) s, M.remove k m
  let remove k (s, m) =
    if M.mem k m then
      let p = M.find k m in
      if P.compare p P.one = 0 then
        S.remove (k, p) s, M.remove k m
      else
        let s = S.remove (k, p) s in
        let p = P.pred p in
        S.add (k, p) s, M.add k p m
    else
      s, m
  let remove_all k (s, m) =
    if M.mem k m then
      let p = M.find k m in
      S.remove (k, p) s, M.remove k m
    else
      s, m
  let of_list l =
    List.fold_left (fun h x -> add x h) empty l
  let to_list (s, _) =
    S.elements s
  let pick f (s, _) =
    let rec loop s =
      if S.is_empty s then
        []
      else
        let k, p = S.min_elt s in
        if f k then
          let rec loop s acc =
            if S.is_empty s then
              List.rev acc
            else
              let k', p' = S.min_elt s in
              if P.compare p p' = 0 then
                if f k' then loop (S.remove (k', p') s) (k' :: acc)
                else loop (S.remove (k', p') s) acc
              else
                List.rev acc
          in
          loop (S.remove (k, p) s) [k]
        else
          loop (S.remove (k, p) s)
    in
    loop s
end

module Int = struct
  type t = int
  let one = 1
  let succ x = x + 1
  let pred x = x - 1
  let compare x y = x - y
end

include Make (Int) (Int)
