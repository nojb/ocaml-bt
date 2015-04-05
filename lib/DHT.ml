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

open Lwt.Infix

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

module KRPC : sig
  type msg =
    | Query of string * (string * Bcode.t) list
    | Response of (string * Bcode.t) list
    | Error of int64 * string

  type rpc =
    | Error
    | Timeout
    | Response of (Unix.inet_addr * int) * (string * Bcode.t) list

  type t

  type answer_func = (Unix.inet_addr * int) -> string -> (string * Bcode.t) list -> msg

  val create : answer_func -> int -> t

  val start : t -> unit

  val string_of_msg : msg -> string

  val send_msg : t -> msg -> (Unix.inet_addr * int) -> rpc Lwt.t
end = struct

  module Log = Log.Make (struct let section = "KRPC" end)

  open Printf

  let alpha = 3

  type msg =
    | Query of string * (string * Bcode.t) list
    | Response of (string * Bcode.t) list
    | Error of int64 * string

  let string_of_msg = function
    | Query (name, args) -> sprintf "query %s(%s)" name (String.concat "," (List.map fst args))
    | Response d -> sprintf "response (%s)" (String.concat "," (List.map fst d))
    | Error (n, s) -> sprintf "error (%Ld,%S)" n s

  let encode t msg =
    let str s = Bcode.String (Cstruct.of_string s) in
    let d = match msg with
      | Query (name, args) ->
          ["y", str "q"; "q", str name; "a", Bcode.Dict args]
      | Response dict ->
          ["y", str "r"; "r", Bcode.Dict dict]
      | Error (code, s) ->
          ["y", str "e"; "e", Bcode.List [Bcode.Int code; str s]]
    in
    let d = ("t", str t) :: d in
    Bcode.encode (Bcode.Dict d)

  let decode s =
    let bc = Bcode.decode s in
    let t = Bcode.to_string (Bcode.find "t" bc) in
    let y = Bcode.to_string (Bcode.find "y" bc) in
    let msg = match y with
      | "q" ->
          let q = Bcode.to_string (Bcode.find "q" bc) in
          let a = Bcode.to_dict (Bcode.find "a" bc) in
          Query (q, a)
      | "r" ->
          let r = Bcode.to_dict (Bcode.find "r" bc) in
          Response r
      | "e" ->
          begin match Bcode.to_list (Bcode.find "e" bc) with
          | Bcode.Int n :: Bcode.String s :: _ -> Error (n, Cstruct.to_string s)
          | _ -> failwith "KRPC.decode: bad fields for 'e' entry"
          end
      | _ ->
          failwith (Printf.sprintf "KRPC.decode: unknown message type y %S" y)
    in
    (t, msg)

  module Assoc2 : sig
    type ('a, 'b, 'c) t
    val create : unit -> ('a, 'b, 'c) t
    val add : ('a, 'b, 'c) t -> 'a -> 'b -> 'c -> unit
    val find : ('a, 'b, 'c) t -> 'a -> 'b -> 'c option
    val remove : ('a, 'b, 'c) t -> 'a -> 'b -> unit
    val clear : ('a, 'b, 'c) t -> unit
    val iter : ('a -> 'b -> 'c -> unit) -> ('a, 'b, 'c) t -> unit
  end = struct
    type ('a, 'b, 'c) t = ('a, ('b, 'c) Hashtbl.t) Hashtbl.t
    let create () = Hashtbl.create 3
    let add h a b c =
      let hh = try Hashtbl.find h a with Not_found -> Hashtbl.create 3 in
      Hashtbl.add hh b c;
      Hashtbl.replace h a hh
    let find h a b =
      try Some (Hashtbl.find (Hashtbl.find h a) b) with Not_found -> None
    let remove h a b =
      try Hashtbl.remove (Hashtbl.find h a) b with Not_found -> ()
    let clear h =
      Hashtbl.clear h
    let iter f h =
      Hashtbl.iter (fun a h -> Hashtbl.iter (fun b c -> f a b c) h) h
  end

  type addr = Unix.inet_addr * int

  type answer_func = addr -> string -> (string * Bcode.t) list -> msg

  type rpc =
    | Error
    | Timeout
    | Response of addr * (string * Bcode.t) list

  type t = {
    sock : Lwt_unix.file_descr;
    port : int;
    answer : answer_func;
    pending : (addr, string, rpc Lwt.u * float) Assoc2.t;
    buf : Cstruct.t
  }

  let max_datagram_size = 1024

  let create answer port =
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    let buf = Cstruct.create max_datagram_size in
    { sock; port; answer; pending = Assoc2.create (); buf }

  let read_one_packet krpc =
    Lwt_cstruct.recvfrom krpc.sock krpc.buf [] >>= fun (n, sa) ->
    let addr = match sa with Unix.ADDR_INET (ip, p) -> (ip, p) | Unix.ADDR_UNIX _ -> assert false in
    let (t, msg) = decode (Cstruct.sub krpc.buf 0 n) in
    match msg with
    | Error (code, msg) ->
        begin match Assoc2.find krpc.pending addr t with
        | None ->
            ()
        (* debug "no t:%S for %s" t (addro_string addr) *)
        | Some (w, _) ->
            Assoc2.remove krpc.pending addr t;
            Lwt.wakeup w Error
        end;
        Lwt.return ()
    | Query (name, args) ->
        let ret = krpc.answer addr name args in
        Lwt_cstruct.sendto krpc.sock (encode t ret) [] sa >>= fun _ ->
        Lwt.return_unit
    | Response args ->
        begin match Assoc2.find krpc.pending addr t with
        | None ->
            (* debug "no t:%S for %s" t (addro_string addr); *)
            Lwt.return_unit
        | Some (w, _) ->
            Assoc2.remove krpc.pending addr t;
            Lwt.wakeup w (Response (addr, args));
            Lwt.return_unit
        end

  let rec read_loop krpc =
    read_one_packet krpc >>= fun () -> read_loop krpc

  let fresh_txn =
    let last = ref 0 in
    fun () ->
      let id = string_of_int !last in
      incr last;
      id

  let timeout_check_delay = 5.0

  let rec timeout_pulse krpc =
    let now = Unix.time () in
    let bad = ref [] in
    Assoc2.iter
      (fun addr t (w, at_most) -> if at_most < now then bad := (addr, t, w) :: !bad)
      krpc.pending;
    List.iter
      (fun (addr, t, w) -> Assoc2.remove krpc.pending addr t; Lwt.wakeup w Timeout)
      !bad;
    Lwt_unix.sleep timeout_check_delay >>= fun () -> timeout_pulse krpc

  let start krpc =
    Lwt.async (fun () -> read_loop krpc);
    Lwt.async (fun () -> timeout_pulse krpc)

  let timeout_delay = 20.0

  let send_msg krpc msg addr =
    let t = fresh_txn () in (* FIXME only outstanding queries need be unique *)
    let wait, w = Lwt.wait () in
    Assoc2.add krpc.pending addr t (w, Unix.time () +. timeout_delay);
    let sa = let ip, p = addr in Unix.ADDR_INET (ip, p) in
    Lwt_cstruct.sendto krpc.sock (encode t msg) [] sa >>= fun _ ->
    wait

end

module Kademlia : sig

  type addr = Unix.inet_addr * int

  type table

  type node_info = SHA1.t * addr

  val create : SHA1.t -> table

  type ping_fun = addr -> (node_info option -> unit) -> unit


  val update : table -> ping_fun -> status -> SHA1.t -> addr -> unit
  (** [update rt ping st id addr] updates the routing table information for [id].
      If [id] already exists, its address is updated with [addr] and marked fresh,
      status set to [status].  If [addr] is found in the bucket but with a different
      id, then the id is replaced by [id], marked fresh and status set to [status].
      If [id] or [addr] is not found in the bucket, and if the bucket has less than
      the maximum allowed number of elements, a new fresh node with status [status] is
      inserted in the bucket.  If the bucket is already maxed out, then if any ``Bad''
      node is found or a node that has been ``Pinged'', but who we have not heard back
      from in a long time, it is thrown out and replaced by a new fresh node with id
      [id] and status [status].  If we get to this stage, then any ``Good'' nodes that
      we have not heard back from in a long time get marked as ``Unknown''.  Finally,
      if no bad nodes or expired pinged ones are found, there are two possibilities.
      If there are no nodes with ``Unknown'' status, we either split the bucket (if
      the bucket contains our own id) or we reject the node.  On the other hand, if
      there are some nodes with ``Unknown'' status, we ping them and wait for the
      responses.  As they arrive we either mark the nodes as ``Good'' (if a response
      was received) or ``Bad'' (if no response was received).  Once all the
      ``Unknown'' nodes have been dealt with, the insertion is tried again. *)

  val find_node : table -> SHA1.t -> node_info list

  val refresh : table -> (SHA1.t * node_info list) list

  val size : table -> int

  val bucket_nodes : int
end = struct

  type addr = Unix.inet_addr * int

  type node_info = SHA1.t * addr (* Addr.t *)

  let node_period = 15.0 *. 60.0

  let bucket_nodes = 8

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
    Log.debug "mark [%s] as %s" (string_of_node n) (string_of_status st);
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
      Log.debug "replace [%s] with %s" (string_of_node b.nodes.(i)) (SHA1.to_hex_short id);
      b.nodes.(i) <- make_node id addr status; (* replace *)
      touch b;
      raise Exit
    end

  let split_bucket b =
    let mid = split b.lo b.hi in
    Log.debug "split [lo %s] [hi %s] [mid %s]" (SHA1.to_hex b.lo) (SHA1.to_hex b.hi) (SHA1.to_hex mid);
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
                Log.debug "bucket full (%s)" (SHA1.to_hex_short id);
                raise Exit
              end
          | _ ->
              let count = ref (List.length unk) in
              Log.debug "ping %d unknown nodes" !count;
              let on_pong n res =
                decr count;
                mark n (match res with Some _ -> Good | None -> Bad);
                if !count = 0 then begin (* retry *)
                  Log.debug "all %d pinged, retry %s" (List.length unk) (SHA1.to_hex_short id);
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
                Log.debug "insert_node: duplicate entry %s" (string_of_node n);
                raise Exit
            | true, false | false, true ->
                Log.debug "insert_node: conflict [%s] with [%s]" (string_of_node n) (string_of_node node);
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
            Log.debug "insert_node: bucket full [%s]" (string_of_node node);
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

  let array_exists f a =
    let rec loop i = (i < Array.length a) && (f a.(i) || loop (i + 1)) in
    loop 0

  let refresh table =
    let now = Unix.time () in
    let rec loop acc = function
      | N (l, _, r) ->
          loop (loop acc l) r
      | L b when b.last_change +. node_period < now ->
          if array_exists (fun n -> n.status <> Bad) b.nodes then
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

end

module Log = Log.Make (struct let section = "DHT" end)

let alpha = 3

type query =
  | Ping
  | FindNode of SHA1.t
  | GetPeers of SHA1.t
  | Announce of SHA1.t * int * string

let string_of_query = function
  | Ping ->
    "ping"
  | FindNode id ->
    Printf.sprintf "find_node %s" (SHA1.to_hex_short id)
  | GetPeers ih ->
    Printf.sprintf "get_peers %s" (SHA1.to_hex_short ih)
  | Announce (ih, port, token) ->
      Printf.sprintf "announce %s %d %S" (SHA1.to_hex_short ih) port token

type addr = Unix.inet_addr * int

type node_info = SHA1.t * addr

type response =
  | Pong
  | Nodes of node_info list
  | Peers of string * addr list * node_info list

let string_of_node (id, (ip, port)) =
  Printf.sprintf "%s (%s:%d)" (SHA1.to_hex_short id) (Unix.string_of_inet_addr ip) port

let strl f l = "[" ^ String.concat " " (List.map f l) ^ "]"

let string_of_response r =
  match r with
  | Pong ->
    "pong"
  | Nodes nodes ->
    Printf.sprintf "nodes %s" (strl string_of_node nodes)
  | Peers (token, peers, nodes) ->
      "" (* FIXME FIXME *)
    (* Printf.sprintf "peers token=%S peers %s nodes %s" *)
      (* token (strl Addr.to_string peers) (strl string_of_node nodes) *)

let parse_query name args : SHA1.t * query =
  let sha1 k args = SHA1.of_raw (Bcode.to_cstruct (List.assoc k args)) in
  let q = match name with
    | "ping" ->
      Ping
    | "find_node" ->
      FindNode (sha1 "target" args)
    | "get_peers" ->
      GetPeers (sha1 "info_hash" args)
    | "announce_peer" ->
      let ih = sha1 "info_hash" args in
      let port = Bcode.to_int (List.assoc "port" args) in
      let token = Bcode.to_string (List.assoc "token" args) in
      Announce (ih, port, token)
    | _ ->
      failwith (Printf.sprintf "DHT.parse_query name:%s" name)
  in
  let id = sha1 "id" args in
  id, q

let parse_nodes s =
  (* let s = Bitstring.bitstring_of_string s in *)
  let rec loop s = []
  (* FIXME FIXME *)
    (* bitmatch s with *)
    (* | { id : 20 * 8 : string, bind (SHA1.of_bin id); *)
    (*     addr : 6 * 8 : bitstring, bind (Addr.of_string_compact addr); *)
    (*     rest : -1 : bitstring } -> *)
    (*   (id, addr) :: loop rest *)
    (* | { _ } -> *)
    (*   [] *)
  in
  loop s

let parse_value s =
  (* let s = Bitstring.bitstring_of_string s in *)
  assert false (* FIXME FIXME *)
  (* Addr.of_string_compact s *)
  (* bitmatch s with *)
  (* let rec loop s = *)
  (*   bitmatch s with *)
  (*   | { addr : 6 * 8 : bitstring, bind (Addr.of_string_compact addr); *)
  (*       rest : -1 : bitstring } -> *)
  (*     addr :: loop rest *)
  (*   | { _ } -> *)
  (*     [] *)
  (* in *)
  (* loop s *)

let parse_response q args =
  let sha1 k args = SHA1.of_raw (Bcode.to_cstruct (List.assoc k args)) in
  let r = match q with
    | Ping ->
      Pong
    | FindNode _ ->
      let s = Bcode.to_string (List.assoc "nodes" args) in
      Nodes (parse_nodes s)
    | GetPeers _ ->
      let token = Bcode.to_string (List.assoc "token" args) in
      let peers =
        try
          let values = Bcode.to_list (List.assoc "values" args) in
          let values = List.map Bcode.to_string values in
          List.map parse_value values
        with
        | Not_found -> []
      in
      let nodes =
        try parse_nodes (Bcode.to_string (List.assoc "nodes" args)) with Not_found -> []
      in
      Peers (token, peers, nodes)
    | Announce _ ->
      Pong
  in
  let id = sha1 "id" args in
  id, r

let secret_timeout = 10.0 *. 60.0

module Secret : sig
  type t
  val create : float -> t
  val current : t -> string
  val previous : t -> string
  val is_valid : t -> string -> bool
end = struct
  type t = {
    mutable cur : string;
    mutable prev : string;
    mutable next_timeout : float;
    timeout_delay : float
  }
  let fresh () = string_of_int (Random.int 1_000_000)
  let create timeout_delay =
    let s = fresh () in
    { cur = s; prev = s; next_timeout = Unix.time () +. timeout_delay; timeout_delay }
  let check_timeout secret =
    let now = Unix.time () in
    if now > secret.next_timeout then begin
      secret.prev <- secret.cur;
      secret.cur <- fresh ();
      secret.next_timeout <- now +. secret.timeout_delay
    end
  let current secret = check_timeout secret; secret.cur
  let previous secret = check_timeout secret; secret.prev
  let is_valid secret str = check_timeout secret; str = secret.cur || str = secret.prev
end

module Peers = Map.Make (struct type t = addr (* Addr.t *) let compare = compare end)

type t = {
  mutable rt : Kademlia.table;
  torrents : (SHA1.t, float Peers.t) Hashtbl.t;
  id : SHA1.t;
  port : int;
  krpc : KRPC.t
}

type query_type =
  | Ping
  | FindNode
  | GetPeers
  | Announce

let type_of_query : query -> query_type = function
  | Ping -> Ping
  | FindNode _ -> FindNode
  | GetPeers _ -> GetPeers
  | Announce _ -> Announce

let encode_query id (q : query) =
  let sha1 x = Bcode.String (SHA1.to_raw x) in
  let self = ("id", sha1 id) in
  match q with
  | Ping ->
    KRPC.Query ("ping", [self])
  | FindNode id ->
    KRPC.Query ("find_node", ["target", sha1 id; self])
  | GetPeers ih ->
    KRPC.Query ("get_peers", ["info_hash", sha1 ih; self])
  | Announce (ih, port, token) ->
    KRPC.Query ("announce",
      [ "info_hash", sha1 ih;
        "port", Bcode.Int (Int64.of_int port);
        "token", Bcode.String (Cstruct.of_string token);
        self ])

let query dht addr q =
  (* FIXME FIXME *)
  (* debug "query: %s %s" (Addr.to_string addr) (string_of_query q); *)
  KRPC.send_msg dht.krpc (encode_query dht.id q) addr >>= function
  | KRPC.Response (addr, args) ->
      let id, r = parse_response q args in
      (* FIXME FIXME *)
    (* debug "query: got response from %s (%s): %s" *)
      (* (SHA1.to_hex_short id) (Addr.to_string addr) (string_of_response r); *)
    Lwt.return ((id, addr), r)
  | KRPC.Timeout ->
    Lwt.fail (Failure "timeout")
  | KRPC.Error ->
    Lwt.fail (Failure "dht error")

let ping dht addr =
  Lwt.catch begin fun () ->
    query dht addr Ping >>= fun (n, r) ->
    match r with
    | Pong ->
      Lwt.return (Some n)
    | _ ->
      Lwt.return None
  end begin fun _ -> Lwt.return None end

let find_node dht addr id =
  query dht addr (FindNode id) >>= fun (n, r) ->
  match r with
  | Nodes nodes ->
    Lwt.return (n, nodes)
  | _ ->
    Lwt.fail (Failure "DHT.find_node")

let get_peers dht addr ih =
  query dht addr (GetPeers ih) >>= fun (n, r) ->
  match r with
  | Peers (token, peers, nodes) ->
    Lwt.return (n, token, peers, nodes)
  | _ ->
    Lwt.fail (Failure "DHT.get_peers")

let announce dht addr port token ih =
  query dht addr (Announce (ih, port, token)) >>= fun (n, r) ->
  match r with
  | Pong ->
    Lwt.return n
  | _ ->
    Lwt.fail (Failure "DHT.announce")

let encode_nodes l =
  (* let rec loop = function *)
  (*   | [] -> Bitstring.empty_bitstring *)
  (*   | (id, addr) :: nodes -> *)
  (*     BITSTRING { SHA1.to_bin id : -1 : string; *)
  (*                 Addr.to_string_compact addr : -1 : string; *)
  (*                 loop nodes : -1 : bitstring } *)
  (* in *)
  (* Bitstring.string_of_bitstring (loop l) *)
  assert false
(* FIXME FIXME *)

let encode_values l =
  assert false
  (* FIXME FIXME *)
  (* let rec loop = function *)
  (*   | [] -> Bitstring.empty_bitstring *)
  (*   | addr :: values -> *)
  (*     BITSTRING { Addr.to_string_compact addr : -1 : string; *)
  (*                 loop values : -1 : bitstring } *)
  (* in *)
  (* Bitstring.string_of_bitstring (loop l) *)

let encode_response id r : KRPC.msg =
  let sha1 x = Bcode.String (SHA1.to_raw x) in
  let self = ("id", sha1 id) in
  match r with
  | Pong ->
    KRPC.Response [ self ]
  | Nodes nodes ->
    KRPC.Response
      [ self; "nodes", Bcode.String (encode_nodes nodes) ]
  | Peers (token, peers, nodes) ->
    KRPC.Response
      [ self; "token", Bcode.String (Cstruct.of_string token);
        "values", Bcode.String (encode_values peers);
        "nodes", Bcode.String (encode_nodes nodes) ]

let shuffle_array a = () (* FIXME FIXME *)

let self_get_peers dht h =
  let peers =
    try
      Peers.fold (fun p _ l -> p :: l) (Hashtbl.find dht.torrents h) []
    with
    | Not_found -> []
  in
  if List.length peers <= 100 then
    peers
  else
    let a = Array.of_list peers in
    shuffle_array a;
    Array.to_list (Array.sub a 0 100)

let self_find_node dht h =
  Kademlia.find_node dht.rt h

(* do not hash port cause some broken implementations change it all the time *)
let make_token addr ih secret =
  "" (* FIXME FIXME *)
  (* string_of_int (Hashtbl.hash *)
      (* (Addr.Ip.to_string (Addr.ip addr), SHA1.to_bin ih, secret)) *)

let valid_token addr ih secret token =
  let cur = Secret.current secret in
  let prev = Secret.previous secret in
  token = make_token addr ih cur || token = make_token addr ih prev

let store_peer_timeout = 30.0 *. 60.0

let store dht ih addr =
  let peers = try Hashtbl.find dht.torrents ih with Not_found -> Peers.empty in
  Hashtbl.replace dht.torrents ih (Peers.add addr (Unix.time () +. store_peer_timeout) peers)

let answer dht secret addr name args =
  let id, q = parse_query name args in
  let r = match q with
    | Ping ->
      Pong
    | FindNode nid ->
        assert false (* FIXME FIXME *)
      (* Nodes (self_find_node dht nid) *)
    | GetPeers ih ->
      let token = make_token addr ih (Secret.current secret) in
      let peers = self_get_peers dht ih in
      let nodes = self_find_node dht ih in
      Log.debug "answer: %d peers and %d nodes" (List.length peers) (List.length nodes);
      assert false (* FIXME FIXME *)
      (* Peers (token, peers, nodes) *)
    | Announce (ih, port, token) ->
      if not (valid_token addr ih secret token) then
        failwith (Printf.sprintf "answer: invalid token %S" token);
      (* FIXME FIXME *)
      (* store dht ih (Addr.ip addr, port); *)
      Pong
  in
  (* FIXME FIXME *)
  (* debug "answer: %s (%s) : %s" *)
    (* (SHA1.to_hex_short id) (Addr.to_string addr) (string_of_response r); *)
  encode_response dht.id r

let update dht st id addr =
  let ping addr k = Lwt.async (fun () -> ping dht addr >|= k) in
  (* FIXME FIXME *)
  assert false
  (* Kademlia.update dht.rt ping st id addr *)

let (!!) = Lazy.force

let create port =
  let id = SHA1.generate () in
  let secret = Secret.create secret_timeout in
  let rec dht = lazy
    { rt = Kademlia.create id;
      krpc = KRPC.create (fun addr name args -> answer !!dht secret addr name args) port;
      port;
      id;
      torrents = Hashtbl.create 3 }
  in
  !!dht

let rec refresh dht =
  let ids = Kademlia.refresh dht.rt in
  Log.debug "refresh: %d buckets" (List.length ids);
  let cb prev_id (n, l) =
    let id, addr = n in
    update dht Good id addr; (* replied *)
    if SHA1.compare id prev_id <> 0 then begin
      Log.debug "refresh: node %s changed id (was %s)" (string_of_node n) (SHA1.to_hex_short prev_id);
      update dht Bad prev_id addr
    end;
    Log.debug "refresh: got %d nodes from %s" (List.length l) (string_of_node n);
    List.iter (fun (id, addr) -> update dht Unknown id addr) l
  in
  assert false (* FIXME FIXME *)
  (* Lwt_list.iter_p (fun (target, nodes) -> *)
  (*   Lwt_list.iter_p (fun n -> *)
  (*     Lwt.catch *)
  (*       (fun () -> let id, addr = n in find_node dht addr target >|= cb id) *)
  (*       (fun exn -> *)
  (*          debug ~exn "refresh: find_node error %s" (string_of_node n); *)
  (*          Lwt.return ())) *)
  (*     nodes) ids >>= fun () -> *)
  (* Lwt_unix.sleep 60.0 >>= fun () -> refresh dht *)

let expire_timer = 60.0

let rec expire_old_peers h =
  let now = Unix.time () in
  let torrents = Hashtbl.fold (fun k peers l -> (k, peers) :: l) h [] in
  let rm = ref 0 in
  let total = ref 0 in
  List.iter (fun (id, peers) ->
    let m =
      Peers.fold begin fun peer expire m ->
        incr total;
        if expire < now then begin
          incr rm;
          Peers.remove peer m
        end
        else m
      end peers peers
    in
    if Peers.is_empty m then Hashtbl.remove h id else Hashtbl.replace h id m) torrents;
  Log.debug "Removed %d of %d peers for announced torrents" !rm !total;
  Lwt_unix.sleep expire_timer >>= fun () -> expire_old_peers h

let start dht =
  Log.debug "DHT size : %d self : %s" (Kademlia.size dht.rt) (SHA1.to_hex_short dht.id);
  KRPC.start dht.krpc;
  Lwt.async (fun () -> refresh dht);
  Lwt.async (fun () -> expire_old_peers dht.torrents)

module BoundedSet = struct
  module type S = sig
    type elt
    type t
    val create : int -> t
    val insert : t -> elt -> bool
    val elements : t -> elt list
    val iter : (elt -> unit) -> t -> unit
    val min_elt : t -> elt
    val is_empty : t -> bool
  end
  module Make (Ord : Set.OrderedType) : S with type elt = Ord.t = struct
    module S = Set.Make (Ord)
    type elt = Ord.t
    type t = int ref * S.t ref

    let create n = ref n, ref S.empty

    let insert (left, set) elem =
      if S.mem elem !set then false else
      if !left = 0 then
        let max = S.max_elt !set in
        if Ord.compare elem max < 0 then begin
          set := S.add elem (S.remove max !set);
          true
        end
        else false
      else begin
        set := S.add elem !set;
        decr left;
        true
      end

    let iter f (_, set) = S.iter f !set

    let elements (_, set) = S.elements !set

    let min_elt (_, set) = S.min_elt !set

    let is_empty (_, set) = S.is_empty !set
  end
end

let lookup_node dht ?nodes target =
  Log.debug "lookup_node: %s" (SHA1.to_hex_short target);
  let start = Unix.time () in
  let queried = Hashtbl.create 13 in
  let module BS = BoundedSet.Make
      (struct
        type t = SHA1.t * addr (* Addr.t *)
        let compare n1 n2 =
          Z.compare (SHA1.distance target (fst n1)) (SHA1.distance target (fst n2))
      end)
  in
  let found = BS.create Kademlia.bucket_nodes in
  let rec loop nodes =
    let inserted =
      List.fold_left (fun acc node ->
        if BS.insert found node then acc+1 else acc) 0 nodes
    in
    let n = ref 0 in
    let res = ref [] in
    begin try
      BS.iter begin fun node ->
        if alpha = !n then raise Exit;
        if not (Hashtbl.mem queried node) then begin
          incr n;
          res := (query true node) :: !res;
        end
      end found
    with
    | Exit -> ()
    end;
    inserted, Lwt.join !res
  and query store n =
    Hashtbl.add queried n true;
    Log.debug "lookup_node: will query node %s" (string_of_node n);
    Lwt.catch
      (fun () ->
         let _, addr = n in
         find_node dht addr target >>= fun (n, nodes) ->
         let id, addr = n in
         if store then update dht Good id addr;
         let inserted, t = loop nodes in
         let s =
           if BS.is_empty found then ""
           else Printf.sprintf ", best %s" (SHA1.to_hex_short (fst (BS.min_elt found)))
         in
         Log.debug "lookup_node: got %d nodes from %s, useful %d%s" (List.length nodes) (string_of_node n) inserted s;
         t)
      (fun exn ->
         Log.debug "lookup_node: timeout from %s" (string_of_node n);
         Lwt.return ())
  in
  begin match nodes with
  | None ->
      (* let _, t = loop (self_find_node dht target) in t *)
      assert false (* FIXME FIXME *)
  | Some nodes ->
    Lwt_list.iter_p (query false) nodes
  end >>= fun () ->
  let result = BS.elements found in
  Log.debug "lookup_node %s done, queried %d, found %d, elapsed %ds"
    (SHA1.to_hex_short target) (Hashtbl.length queried) (List.length result)
    (truncate (Unix.time () -. start));
  Lwt.return result

let query_peers dht id k =
  Log.debug "query_peers: start %s" (SHA1.to_hex_short id);
  lookup_node dht id >>= fun nodes ->
  Log.debug "query_peers: found nodes %s" (strl string_of_node nodes);
  Lwt_list.iter_p begin fun n ->
    Lwt.catch
      (fun () ->
         get_peers dht (snd n) id >|= fun (node, token, peers, nodes) ->
         Log.debug "query_peers: got %d peers and %d nodes from %s with token %S"
           (List.length peers) (List.length nodes) (string_of_node node) token;
         k n token peers)
      (fun exn ->
         Log.debug "query_peers: get_peers error from %s : %S" (string_of_node n) (Printexc.to_string exn);
         Lwt.return_unit)
  end nodes

let bootstrap dht addr =
  ping dht addr >>= function
  | Some n ->
      (* debug "bootstrap node %s (%s) is up" (string_of_node n) (Addr.to_string addr); *)
      (* FIXME FIXME *)
    lookup_node dht ~nodes:[n] dht.id >>= fun l ->
    (* debug "bootstrap via %s : found %s" (Addr.to_string addr) (strl string_of_node l); *)
    (* FIXME FIXME *)
    Lwt.return (List.length l >= Kademlia.bucket_nodes)
  | None ->
      (* debug "bootstrap node %s is down" (Addr.to_string addr); *)
      (* FIXME FIXME *)
    Lwt.return false

let bootstrap dht (host, port) =
  (* Lwt.catch *)
  (*   (fun () -> Addr.Ip.of_string_noblock host >>= fun ip -> bootstrap dht (ip, port)) *)
  (*   (fun exn -> debug ~exn "bootstrap error"; Lwt.return false) *)
  assert false (* FIXME FIXME *)

let rec auto_bootstrap dht routers =
  lookup_node dht dht.id >>= fun l ->
  Log.debug "auto bootstrap : found %s" (strl string_of_node l);
  let rec loop l ok =
    match l, ok with
    | _, true ->
      Log.debug "bootstrap ok, total nodes : %d" (Kademlia.size dht.rt);
      Lwt.return ()
    | [], false ->
      Log.debug "bootstrap failed, total nodes : %d; retrying" (Kademlia.size dht.rt);
      auto_bootstrap dht routers
    | n :: ns, false ->
      bootstrap dht n >>= loop ns
  in
  loop routers (List.length l >= Kademlia.bucket_nodes)
  (* Lwt_list.iter_p (fun addr -> bootstrap dht addr >>= fun _ -> Lwt.return ()) routers *)
  (* let rec loop l = *)

  (*   match l, ok with *)
  (*   | _, true -> *)
  (*     info "bootstrap ok, total nodes : %d" (Kademlia.size dht.rt); *)
  (*     Lwt.return () *)
  (*   | [], false -> *)
  (*     info "bootstrap failed, total nodes : %d" (Kademlia.size dht.rt); *)
  (*     Lwt.return () *)
  (*   | (n :: nodes), false -> *)
  (*     bootstrap dht n >>= loop nodes *)
  (* in *)
  (* loop routers (List.length l >= Kademlia.bucket_nodes) *)

let bootstrap_nodes =
  (* [ "router.utorrent.com", 6881; *)
  (* "router.transmission.com", 6881; *)
  [ "router.bittorrent.com", 6881;
    "dht.transmissionbt.com", 6881 ]
