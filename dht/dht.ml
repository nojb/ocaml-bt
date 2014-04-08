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

val default_config : config

type push_type =
  | Stop
  | RequestPeers of Hash.t * bool * (Udp.address -> unit)
  | Ping of remote_node
  | AddNode of Udp.address
  | RequestNode of Hash.t * bool
  (* | GotPeers of Udp.address list *)
        
type t = {
  node_id : Hash.t;
  config : config;
  routing_table : Routing_table.t;
  peer_store : Peer_store.t;
  conn : Lwt_unix.file_descr;
  explored_neighborhood : bool;
  chan : push_type Lwt_stream.t;
  push : push_type -> unit;
  store : dht_store;
  token_secrets : string array;
  got_peers : Udp.address list -> unit
}

val create : ?config:config -> (Udp.address -> unit) -> t
val add_node : t -> Unix.inet_addr -> int -> unit
val request_peers : t -> ?announce:bool -> Hash.t -> unit
val port : t -> int
val run : t -> unit
val stop : t -> unit

let stop dht =
  dht.push Stop

let request_peers dht ?(announce = true) ih =
  dht.push (RequestPeers (announce, ih))

let add_node dht addr =
  dht.push (AddNode addr)

let got_peers dht addrs =
  dht.got_peers addrs

let got_udp_packet dht (data, addr) =
  dht.push (UdpPacket (data, addr))

let process_packet dht data addr =
  let bc = Get.run Bcode.bdecode data in
  match find "y" bc with
  | "r" ->
    begin match Krpc.Response.decode bc with
      f0
    end
  | "q" ->
    0
  | "e" ->
    0

let run dht =
  let socket_chan, stop_listen = Udp.listen_on dht.config.address dht.config.port push_socket in
  Lwt_log.info_f "DHT: starting DHT node %x on port %d." dht.node_id dht.config.port >>= fun () ->
  let rec loop () =
    Lwt_stream.next dht.chan >>= function
    | Stop ->
      Lwt_log.infof "DHT exiting." >>= fun () ->
      Lwt.return ()
    | NewRemoteNode addr ->
      hello_from_peer addr >>= fun () ->
      loop ()
    | PeerRequest (announce, ih) ->

    | UdpPacket (data, addr) ->
      process_packet dht data addr >>= fun () ->
      loop ()
  in
  loop ()
