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

let section = Log.make_section "Kademlia"

let debug ?exn fmt = Log.debug section ?exn fmt

type addr = Unix.inet_addr * int

type node_info = SHA1.t * addr (* Addr.t *)

let node_period = 15.0 *. 60.0

let bucket_nodes = 8

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
  addr : addr;
  mutable last : float;
  mutable status : status;
}

let string_of_node n =
  assert false (* FIXME FIXME *)
  (* Printf.sprintf "%s at %s was %s %d sec ago" *)
  (*   (SHA1.to_hex_short n.id) *)
  (*   (\* (Addr.to_string n.addr) *\) *)
  (*   (string_of_status n.status) *)
  (*   (truncate (Unix.time () -. n.last)) *)

type bucket = {
  lo : SHA1.t;
  hi : SHA1.t;
  mutable last_change : float;
  mutable nodes : node array
}

let mark n st =
  debug "mark [%s] as %s" (string_of_node n) (string_of_status st);
  n.last <- Unix.time ();
  n.status <- st

let touch b =
  b.last_change <- Unix.time ()

type tree =
  | L of bucket
  | N of tree * SHA1.t * tree

type table = {
  mutable root : tree;
  self : SHA1.t
}

(* boundaries inclusive *)
let inside n h =
  not (SHA1.compare h n.lo < 0 || SHA1.compare h n.hi > 0)

let split lo hi =
  assert (SHA1.compare lo hi < 0);
  let mid = Z.div (Z.add (SHA1.to_z lo) (SHA1.to_z hi)) (Z.of_int 2) in
  SHA1.of_z mid

let succ h =
  assert (SHA1.compare h SHA1.last < 0);
  SHA1.of_z (Z.add (SHA1.to_z h) Z.one)

let choose_random lo hi =
  assert (SHA1.compare lo hi < 0);
  let rec loop a b =
    if SHA1.compare a b = 0 then a
    else
      let mid = split a b in
      if Random.bool () then loop a mid else loop mid b
  in
  loop lo hi

let make_node id addr status =
  { id; addr; status; last = Unix.time () }

let update_bucket_node b status id addr i n =
  match SHA1.compare n.id id = 0, n.addr = addr with
  | true, true ->
    mark n status;
    touch b;
    raise Exit
  | true, false | false, true ->
    (* debug "conflict [%s] with %s %s, replacing" *)
      (* (string_of_node n) (SHA1.to_hex_short id) (Addr.to_string addr); *)
    b.nodes.(i) <- make_node id addr status;
    touch b;
    raise Exit
  | _ ->
    ()

let insert_bucket_node b status id addr =
  (* debug "insert %s %s" (SHA1.to_hex_short id) (Addr.to_string addr); *)
  b.nodes <- Array.of_list (make_node id addr status :: Array.to_list b.nodes);
  touch b;
  raise Exit

let replace_bucket_node b status id addr i n =
  let now = Unix.time () in
  if n.status = Good && now -. n.last > node_period then mark n Unknown;
  if n.status = Bad || (n.status = Pinged && now -. n.last > node_period) then begin
    debug "replace [%s] with %s" (string_of_node b.nodes.(i)) (SHA1.to_hex_short id);
    b.nodes.(i) <- make_node id addr status; (* replace *)
    touch b;
    raise Exit
  end

let split_bucket b =
  let mid = split b.lo b.hi in
  debug "split [lo %s] [hi %s] [mid %s]" (SHA1.to_hex b.lo) (SHA1.to_hex b.hi) (SHA1.to_hex mid);
  let nodes1, nodes2 = List.partition (fun n -> SHA1.compare n.id mid < 0) (Array.to_list b.nodes) in
  (* FIXME *)
  let n1 = { lo = b.lo; hi = mid; last_change = b.last_change; nodes = Array.of_list nodes1 } in
  let n2 = { lo = succ mid; hi = b.hi; last_change = b.last_change; nodes = Array.of_list nodes2 } in
  N (L n1, mid, L n2)

type ping_fun = addr -> (node_info option -> unit) -> unit

let rec update table (ping : ping_fun) status id addr =
  let rec loop = function
    | N (l, mid, r) ->
      let c = SHA1.compare id mid in
      if c < 0 || c = 0 then N (loop l, mid, r) else N (l, mid, loop r)
    | L b ->
      Array.iteri (update_bucket_node b status id addr) b.nodes;
      if Array.length b.nodes <> bucket_nodes then insert_bucket_node b status id addr;
      Array.iteri (replace_bucket_node b status id addr) b.nodes;
      let unknown n = match n.status with Unknown -> true | _ -> false in
      let unk = Array.fold_left (fun acc n -> if unknown n then n :: acc else acc) [] b.nodes in
      match unk with
      | [] -> (* all nodes are good or waiting to pong *)
        if inside b table.self && Z.gt (SHA1.distance b.lo b.hi) (Z.of_int 256) then
          split_bucket b
        else begin
          debug "bucket full (%s)" (SHA1.to_hex_short id);
          raise Exit
        end
      | _ ->
        let count = ref (List.length unk) in
        debug "ping %d unknown nodes" !count;
        let on_pong n res =
          decr count;
          mark n (match res with Some _ -> Good | None -> Bad);
          if !count = 0 then begin (* retry *)
            debug "all %d pinged, retry %s" (List.length unk) (SHA1.to_hex_short id);
            touch b;
            update table ping status id addr
          end
        in
        List.iter (fun n -> mark n Pinged; ping n.addr (on_pong n)) unk;
        raise Exit
  in
  if id <> table.self then
    try
      while true do table.root <- loop table.root done
    with
    | Exit -> ()

let insert_node table node =
  let rec loop = function
    | N (l,mid,r) ->
      let c = SHA1.compare node.id mid in
      if c < 0 || c = 0 then N (loop l, mid, r) else N (l, mid, loop r)
    | L b ->
      Array.iter begin fun n ->
        match SHA1.compare n.id node.id = 0, n.addr = node.addr with
        | true, true ->
          debug "insert_node: duplicate entry %s" (string_of_node n);
          raise Exit
        | true, false | false, true ->
          debug "insert_node: conflict [%s] with [%s]" (string_of_node n) (string_of_node node);
          raise Exit
        | _ -> ()
      end b.nodes;
      if Array.length b.nodes <> bucket_nodes then begin
        b.nodes <- Array.of_list (node :: Array.to_list b.nodes);
        raise Exit
      end;
      if inside b table.self && Z.gt (SHA1.distance b.lo b.hi) (Z.of_int 256) then begin
        let mid = split b.lo b.hi in
        let nodes1, nodes2 =
          List.partition (fun n -> SHA1.compare n.id mid < 0) (Array.to_list b.nodes)
        in
        let last_change = List.fold_left (fun acc n -> max acc n.last) 0.0 in
        let n1 =
          { lo = b.lo; hi = mid;
            last_change = last_change nodes1;
            nodes = Array.of_list nodes1 }
        in
        let n2 =
          { lo = succ mid; hi = b.hi;
            last_change = last_change nodes2;
            nodes = Array.of_list nodes2 }
        in
        N (L n1, mid, L n2)
      end
      else begin
        debug "insert_node: bucket full [%s]" (string_of_node node);
        raise Exit
      end
  in
  try
    while true do table.root <- loop table.root done
  with
  | Exit -> ()

let find_node table id =
  let rec loop alt = function
    | N (l, mid, r) ->
      let c = SHA1.compare id mid in
      if c < 0 || c = 0 then loop (r :: alt) l else loop (l :: alt) r
    | L b ->
      let found = Array.to_list b.nodes in
      List.map (fun n -> n.id, n.addr) found
      (* found *)
      (* if Array.length b.nodes = bucket_nodes then found else found *)
  in
  loop [] table.root

let refresh table =
  let now = Unix.time () in
  let rec loop acc = function
    | N (l, _, r) ->
      loop (loop acc l) r
    | L b when b.last_change +. node_period < now ->
      if Util.array_exists (fun n -> n.status <> Bad) b.nodes then
        let nodes = Array.map (fun n -> n.id, n.addr) b.nodes in
        (choose_random b.lo b.hi, Array.to_list nodes) :: acc
      else
        acc (* do not refresh buckets with all bad nodes *)
    | L _ ->
      acc
  in
  loop [] table.root

let rec fold f acc = function
  | N (l, _, r) -> fold f (fold f acc l) r
  | L b -> f acc b

let size table =
  fold (fun acc b -> acc + Array.length b.nodes) 0 table.root

let create self =
  { root = L { lo = SHA1.zero; hi = SHA1.last; last_change = Unix.time (); nodes = [| |] };
    self }
