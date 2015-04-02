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

(** Distributed Hash Table (DHT).  Shamelessly based on MLdonkey's
    implementation <http://mldonkey.sourceforge.net>. *)

type addr = Unix.inet_addr * int

type node_info = SHA1.t * addr

type t

val ping : t -> addr -> node_info option Lwt.t
(** [ping dht addr] send a [ping] message to [addr].  Returns None if an error
    occurs or if no answer is received for too long.  Otherwise it returns the
    contact information for the responding node.  This function never fails. *)

val find_node : t -> addr -> SHA1.t -> (node_info * node_info list) Lwt.t
(** [find_node dht addr id] sends a [find_node] message to [addr].  It can
    fail, or return the contact information of the responding node, and the
    contact information of the node with id [id] (if known) or the 8 closest nodes
    in the routing table of the responding node. *)

val get_peers : t -> addr -> SHA1.t -> (node_info * string * addr list * node_info list) Lwt.t
(** [get_peers dht addr ih] sends a [get_peers] message to [addr].  It can fail,
    or return a tuple [(n, t, values, nodes)], where [n] is the contact
    information for the responding node, [t] is a secret token to be used with
    {!announce}, [values] are the peers that are sharing the torrent with
    info-hash [ih], and [nodes] are the 8 closest nodes to [ih] in the routing
    table of the responding node.  Either [values] or [nodes] can potentially be
    empty. *)

val announce : t -> addr -> int -> string -> SHA1.t -> node_info Lwt.t
(** [announce dht addr port token ih] announces to [addr] that we are sharing
    the torrent with info-hash [ih], [port] is our dht port, [t] is the secret token
    received from a previous call to [get_peers].  This call can fail, or it can
    return the contact information of the responding node. *)

val query_peers : t -> SHA1.t -> (node_info -> string -> addr list -> unit) -> unit Lwt.t
(** [query_peers dht ih k] tries to find peers that are sharing the info-hash
    [ih].  For each set of received peers, [k] is called with the contact
    information of the responding node, the token received from the corresponding
    call to [get_peers], and the list of received peers.  This function cannot fail. *)

val create : int -> t
(** Create a new DHT node listening on the given port. *)

val start : t -> unit
(** Start the event loop. *)

val update : t -> Kademlia.status -> SHA1.t -> addr -> unit
(** [update dht st id addr] updates the status of the DHT node with contact
    information [(id, addr)] to be [st].  If the node does not exist, it is added. *)

val auto_bootstrap : t -> (string * int) list -> unit Lwt.t
(** Bootstrap the DHT node from a list of pairs [(host, port)].  These nodes
    will not be added to the routing table. *)

val bootstrap_nodes : (string * int) list
(** A list of predefined bootstrap nodes. *)
