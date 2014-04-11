open Lwt
  
module Hash = Word160

type config = {
  address : string option;
  port : int option;
  num_target_peers : int;
  dht_routers : (string * int) list;
  max_nodes : int;
  cleanup_period : float;
  save_routing_table : bool;
  save_period : float;
  rate_limit : int
}

(* type remote_node *)

type action =
  | Stop
  | RequestPeers of Hash.t * bool
  | Ping of remote_node
  | AddNode of Udp.address
  | RequestNode of Hash.t * bool
  | UdpPacket of string * Udp.address
        
type t = {
  rt : Rtable.t;
  rpc : Krpc.t;
  
  node_id : Hash.t;
  config : config;
  (* routing_table : Routing_table.t; *)
  (* peer_store : Peer_store.t; *)
  conn : Lwt_unix.file_descr;
  explored_neighborhood : bool;
  chan : push_type Lwt_stream.t;
  push : push_type -> unit;
  (* store : dht_store; *)
  token_secrets : string array;
  got_peers : Udp.address list -> unit
}

(* val create : ?config:config -> (Udp.address -> unit) -> t *)
(* val add_node : t -> Unix.inet_addr -> int -> unit *)
(* val request_peers : t -> ?announce:bool -> Hash.t -> unit *)
(* val port : t -> int *)
(* val run : t -> unit *)
(* val stop : t -> unit *)

let stop dht =
  dht.push Stop

let request_peers dht ?(announce = true) ih =
  dht.push (RequestPeers (ih, announce))

let add_node dht addr =
  dht.push (AddNode addr)

let got_peers dht addrs =
  dht.got_peers addrs

let got_udp_packet dht (data, addr) =
  dht.push (UdpPacket (data, addr))

exception Malformed_packet of string

let string k d = try List.assoc k d with Not_found -> raise (Malformed_packet "process_packet")

module Assoc2 : sig
  type ('a, 'b, 'c) t
  val create : unit -> ('a, 'b, 'c) t
  val add : ('a, 'b, 'c) t -> 'a -> 'b -> 'c -> unit
  val find : ('a, 'b, 'c) t -> 'a -> 'b -> 'c option
  val clear : ('a, 'b, 'c) t -> unit
end = struct
  type ('a, 'b, 'c) t = ('a, ('b, 'c) Hashtbl.t) Hashtbl.t
  let create () = Hashtbl.create ()
  let add h a b c =
    let hh = try Hashtbl.find a h with Not_found -> Hashtbl.create 3 in
    Hashtbl.replace hh b c
  let find h a b =
    try Some (Hashtbl.find (Hashtbl.find h a) b) with Not_found -> None
  let clear h =
    Hashtbl.clear h
end

type query =
  | Ping
  | FindNode of Hash.t
  | GetPeers of Hash.t
  | Announce of Hash.t * int * string

type response =
  | Ack
  | Nodes of Udp.address list
  | Peers of (Hash.t * Udp.address) list

let process_packet dht data addr =
  let module K = Krpc in
  let (txn, msg) = K.decode_exn data in
  match msg with
  | K.Query (name, args) ->
    assert false
  | K.Response d ->
    let id = string "id" d in
    let node, addr = dht.routing_table.node_of_address addr in
    if Remote_node.id node = None then Routing_table.update dht.routing_table node;
    let (query, ok) = Hashtbl.find node.pending_queries txn in
    if !node.
  | K.Error _ ->
    assert false

let run dht =
  (* let socket_chan, stop_listen = Udp.listen_on dht.config.address dht.config.port in *)
  (* Lwt_log.info_f "DHT: starting DHT node %x on port %d." dht.node_id dht.config.port >>= fun () -> *)
  let rec loop () =
    Lwt_stream.next dht.chan >>= function
    | Stop ->
      Lwt_log.info_f "DHT exiting." >>= fun () ->
      Lwt.return ()
    | AddNode addr ->
      (* hello_from_peer addr >>= fun () -> *)
      loop ()
    | RequestPeers (announce, ih) ->
      assert false
    | UdpPacket (data, addr) ->
      (* process_packet dht data addr >>= fun () -> *)
      loop ()
  in
  loop ()
