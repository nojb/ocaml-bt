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

let section = Log.make_section "DHT"

let debug ?exn fmt = Log.debug section ?exn fmt

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let alpha = 3
    
type query =
  | Ping
  | FindNode of SHA1.t
  | GetPeers of SHA1.t
  | Announce of SHA1.t * int * string

type node_info = SHA1.t * Addr.t

type peers =
  | Values of Addr.t list
  | Nodes of node_info list

type response =
  | Pong
  | Nodes of node_info list
  | Peers of string * peers

let string_of_node (id, addr) =
  Printf.sprintf "%s (%s)" (SHA1.to_hex_short id) (Addr.to_string addr)

let string_of_response r =
  let strl f l = "[" ^ String.concat " " (List.map f l) ^ "]" in
  match r with
  | Pong ->
    "pong"
  | Nodes nodes ->
    Printf.sprintf "nodes %s" (strl string_of_node nodes)
  | Peers (token, Values values) ->
    Printf.sprintf "peers token=%S values %s" token (strl Addr.to_string values)
  | Peers (token, Nodes nodes) ->
    Printf.sprintf "peers token=%S nodes %s" token (strl string_of_node nodes)

let parse_query name args : SHA1.t * query =
  let sha1 k args = SHA1.from_bin (Bcode.to_string (List.assoc k args)) in
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
  let s = Bitstring.bitstring_of_string s in
  let rec loop s =
    bitmatch s with
    | { id : 20 * 8 : string, bind (SHA1.from_bin id);
        addr : 6 * 8 : bitstring, bind (Addr.of_string_compact addr);
        rest : -1 : bitstring } ->
      (id, addr) :: loop rest
    | { _ } ->
      []
  in
  loop s

let parse_values s =
  let s = Bitstring.bitstring_of_string s in
  let rec loop s =
    bitmatch s with
    | { addr : 6 * 8 : bitstring, bind (Addr.of_string_compact addr);
        rest : -1 : bitstring } ->
      addr :: loop rest
    | { _ } ->
      []
  in
  loop s

let parse_response q args =
  let sha1 k args = SHA1.from_bin (Bcode.to_string (List.assoc k args)) in
  let r = match q with
    | Ping ->
      Pong
    | FindNode _ ->
      let s = Bcode.to_string (List.assoc "nodes" args) in
      Nodes (parse_nodes s)
    | GetPeers _ ->
      let token = Bcode.to_string (List.assoc "token" args) in
      let peers_or_nodes : peers =
        try
          let values = Bcode.to_string (List.assoc "values" args) in
          Values (parse_values values)
        with
        | Not_found ->
          let nodes = Bcode.to_string (List.assoc "nodes" args) in
          Nodes (parse_nodes nodes)
      in
      Peers (token, peers_or_nodes)
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

module Peers = Map.Make (struct type t = Addr.t let compare = compare end)

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
  let sha1 x = Bcode.String (SHA1.to_bin x) in
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
        "token", Bcode.String token;
        self ])

let query dht addr q =
  KRPC.send_msg dht.krpc (encode_query dht.id q) addr >>= function
  | KRPC.Response (addr, args) ->
    let id, r = parse_response q args in
    Lwt.return ((id, addr), r)
  | KRPC.Timeout ->
    Lwt.fail (Failure "timeout")
  | KRPC.Error ->
    Lwt.fail (Failure "dht error")

let ping dht addr =
  query dht addr Ping >>= fun (n, r) ->
  match r with
  | Pong ->
    Lwt.return (Some n)
  | _ ->
    Lwt.return None

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
  | Peers (token, peers) ->
    Lwt.return (n, token, peers)
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
  let rec loop = function
    | [] -> Bitstring.empty_bitstring
    | (id, addr) :: nodes ->
      BITSTRING { SHA1.to_bin id : -1 : string;
                  Addr.to_string_compact addr : -1 : string;
                  loop nodes : -1 : bitstring }
  in
  Bitstring.string_of_bitstring (loop l)

let encode_values l =
  let rec loop = function
    | [] -> Bitstring.empty_bitstring
    | addr :: values ->
      BITSTRING { Addr.to_string_compact addr : -1 : string;
                  loop values : -1 : bitstring }
  in
  Bitstring.string_of_bitstring (loop l)

let encode_response id r : KRPC.msg =
  let sha1 x = Bcode.String (SHA1.to_bin x) in
  let self = ("id", sha1 id) in
  match r with
  | Pong ->
    KRPC.Response [ self ]
  | Nodes nodes ->
    KRPC.Response
      [ self; "nodes", Bcode.String (encode_nodes nodes) ]
  | Peers (token, Values values) ->
    KRPC.Response
      [ self; "token", Bcode.String token; "values", Bcode.String (encode_values values) ]
  | Peers (token, Nodes nodes) ->
    KRPC.Response
      [ self; "token", Bcode.String token; "nodes", Bcode.String (encode_nodes nodes) ]

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
    Util.shuffle_array a;
    Array.to_list (Array.sub a 0 100)

let self_find_node dht h =
  Kademlia.find_node dht.rt h

(* do not hash port cause some broken implementations change it all the time *)
let make_token addr ih secret =
  string_of_int (Hashtbl.hash
      (Addr.Ip.to_string (Addr.ip addr), SHA1.to_bin ih, secret))

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
      Nodes (self_find_node dht nid)
    | GetPeers ih ->
      let token = make_token addr ih (Secret.current secret) in
      begin match self_get_peers dht ih with
      | [] ->
        let nodes = self_find_node dht ih in
        debug "answer with %d nodes" (List.length nodes);
        Peers (token, Nodes nodes)
      | _ as peers ->
        debug "answer with %d peers" (List.length peers);
        Peers (token, Values peers)
      end
    | Announce (ih, port, token) ->
      if not (valid_token addr ih secret token) then
        failwith (Printf.sprintf "invalid token %S" token);
      store dht ih (Addr.ip addr, port);
      Pong
  in
  debug "DHT response to %s (%s) : %s"
    (SHA1.to_hex_short id) (Addr.to_string addr) (string_of_response r);
  encode_response dht.id r

let update dht st id addr =
  let ping addr k = Lwt.async (fun () -> ping dht addr >|= k) in
  Kademlia.update dht.rt ping st id addr

let (!!) = Lazy.force

let create port =
  let id = SHA1.random () in
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
  debug "will refresh %d buckets" (List.length ids);
  let cb prev_id (n, l) =
    let id, addr = n in
    update dht Kademlia.Good id addr; (* replied *)
    if SHA1.compare id prev_id <> 0 then begin
      debug "refresh: node %s (%s) changed id (was %s)"
        (SHA1.to_hex_short id) (Addr.to_string addr) (SHA1.to_hex_short prev_id);
      update dht Kademlia.Bad prev_id addr
    end;
    debug "refresh: got %d nodes from %s (%s)"
      (List.length l) (SHA1.to_hex_short id) (Addr.to_string addr);
    List.iter (fun (id, addr) -> update dht Kademlia.Unknown id addr) l
  in
  Lwt_list.iter_p (fun (target, nodes) ->
    Lwt_list.iter_p (fun (id, addr) ->
      Lwt.catch
        (fun () -> find_node dht addr target >|= cb id)
        (fun exn ->
           debug ~exn "refresh: find_node error %s (%s)" (SHA1.to_hex_short id) (Addr.to_string addr);
           Lwt.return ()))
      nodes) ids >>= fun () ->
  Lwt_unix.sleep 60.0 >>= fun () -> refresh dht

let start dht =
  debug "DHT size : %d self : %s" (Kademlia.size dht.rt) (SHA1.to_hex_short dht.id);
  Lwt.async (fun () -> refresh dht)

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
  debug "lookup %s" (SHA1.to_hex_short target);
  let start = Unix.time () in
  let queried = Hashtbl.create 13 in
  let module BS = BoundedSet.Make
      (struct
        type t = SHA1.t * Addr.t
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
    let res = ref (Lwt.return ()) in
    let t =
      try
        BS.iter begin fun node ->
          if alpha = !n || Hashtbl.mem queried node then raise Exit;
          incr n;
          res := Lwt.join [!res; query true node]
        end found;
        !res
      with
      | Exit -> !res
    in
    inserted, t
  and query store n =
    let id, addr = n in
    Hashtbl.add queried n true;
    debug "will query node %s (%s)" (SHA1.to_hex_short id) (Addr.to_string addr);
    Lwt.catch
      (fun () ->
         find_node dht addr target >>= fun (n, nodes) ->
         let id, addr = n in
         if store then update dht Kademlia.Good id addr;
         let inserted, t = loop nodes in
         let s =
           if BS.is_empty found then ""
           else Printf.sprintf ", best %s" (SHA1.to_hex_short (fst (BS.min_elt found)))
         in
         debug "got %d nodes from %s (%s), useful %d%s"
           (List.length nodes) (SHA1.to_hex_short id) (Addr.to_string addr)
           inserted s;
         t)
      (fun exn ->
         debug "timeout from %s (%s)" (SHA1.to_hex_short id) (Addr.to_string addr);
         Lwt.return ())
  in
  begin match nodes with
  | None ->
    let _, t = loop (self_find_node dht target) in t
  | Some nodes ->
    Lwt_list.iter_p (query false) nodes
  end >>= fun () ->
  let result = BS.elements found in
  debug "lookup_node %s done, queried %d, found %d, elapsed %ds"
    (SHA1.to_hex_short target) (Hashtbl.length queried) (List.length result)
    (truncate (Unix.time () -. start));
  Lwt.return result
