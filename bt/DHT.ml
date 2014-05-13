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

let (>>=) = Lwt.(>>=)

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
  mutable table : Kademlia.t;
  torrents : (SHA1.t, int Peers.t) Hashtbl.t;
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
  | KRPC.Response args ->
    let id, r = parse_response q args in
    Lwt.return (id, r)
  | KRPC.Timeout ->
    Lwt.fail (Failure "timeout")
  | KRPC.Error ->
    Lwt.fail (Failure "dht error")

let ping dht addr =
  query dht addr Ping >>= fun (id, r) ->
  match r with
  | Pong ->
    Lwt.return (Some id)
  | _ ->
    Lwt.return None

let find_node dht addr id =
  query dht addr (FindNode id) >>= fun (id, r) ->
  match r with
  | Nodes nodes ->
    Lwt.return (id, nodes)
  | _ ->
    Lwt.fail (Failure "DHT.find_node")

let get_peers dht addr ih =
  query dht addr (GetPeers ih) >>= fun (id, r) ->
  match r with
  | Peers (token, peers) ->
    Lwt.return (id, token, peers)
  | _ ->
    Lwt.fail (Failure "DHT.get_peers")

let announce dht addr port token ih =
  query dht addr (Announce (ih, port, token)) >>= fun (id, r) ->
  match r with
  | Pong ->
    Lwt.return id
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

let answer dht addr name args =
  let id, q = parse_query name args in
  let r = match q with
    | Ping ->
      Pong
    | FindNode nid ->
      (* Nodes (M.self_find_node dht nid) *)
    (* | _ -> *)
      assert false
  in
  encode_response dht.id r

let update dht st id addr =
  Kademlia.update dht.table (ping dht) st id addr

let (!!) = Lazy.force

let create port =
  let rec dht = lazy
    { table = Kademlia.create ();
      krpc = KRPC.create (fun addr name args -> answer !!dht addr name args) port;
      port;
      id = SHA1.peer_id "OCTO";
      torrents = Hashtbl.create 3 }
  in
  !!dht

let start dht =
  let secret = Secret.create secret_timeout in
  assert false
