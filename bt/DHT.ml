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

type nodes =
  | Exact of node_info
  | Closest of node_info list

type peers =
  | Peers of Addr.t list
  | Closest of node_info list

type response =
  | Pong
  | Nodes of nodes
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
      begin match parse_nodes s with
      | node :: [] -> Nodes (Exact node)
      | _ as nodes -> Nodes (Closest nodes)
      end
    | GetPeers _ ->
      let token = Bcode.to_string (List.assoc "token" args) in
      let peers_or_nodes : peers =
        try
          let values = Bcode.to_string (List.assoc "values" args) in
          Peers (parse_values values)
        with
        | Not_found ->
          let nodes = Bcode.to_string (List.assoc "nodes" args) in
          Closest (parse_nodes nodes)
      in
      Peers (token, peers_or_nodes)
    | Announce _ ->
      Pong
  in
  let id = sha1 "id" args in
  id, r

module Peers = Map.Make (struct type t = Addr.t let compare = compare end)

type node = unit
  
type t = {
  mutable table : node Kademlia.t;
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

let create answer port id =
  { table = Kademlia.empty;
    krpc = KRPC.create answer port;
    port;
    id;
    torrents = Hashtbl.create 3 }
