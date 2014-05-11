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

(** Tracker *)

type event =
  | STARTED
  | STOPPED
  | COMPLETED

exception Error of string
exception Warning of string

type response = {
  peers : Addr.t list;
  (** The contact information for peers. *)
  leechers : int option;
  (** How many peers have not yet the whole torrent. *)
  seeders : int option;
  (** How many peers have the whole torrent. *)
  interval : int
  (** The desired interval (in seconds) before another announce. *)
}

val query :
  Uri.t ->
  ih:SHA1.t ->
  ?up:int64 ->
  ?down:int64 ->
  ?left:int64 ->
  ?event:event ->
  port:int ->
  id:SHA1.t -> response Lwt.t
(** [query url ih up down left event port id] announces a torrent with info-hash
    [ih] to the tracker [url].  [up] is the number of bytes uploaded since we last
    restarted the torrent.  [down] is the number of bytes downloaded since we last
    restarted the torrent.  [left] is the number of bytes currently left to
    download.  [port] is the port in which we are listening for incoming
    connections, [id] is our client ID. *)

module Tier : sig
  type t

  exception No_valid_tracker

  val create : unit -> t
  (** Create an empty tier. *)
    
  val shuffle : t -> unit
  (** Shuffle a tier randomly. *)
    
  val add_tracker : t -> Uri.t -> unit
  (** Add a tracker to a tier. *)
    
  val query : t -> ih:SHA1.t -> ?up:int64 -> ?down:int64 -> ?left:int64 -> ?event:event ->
    port:int -> id:SHA1.t -> response Lwt.t
  (** Query a tier.  All the trackers are tried in turn, until one that works is
      found. *)
      
  val show : t -> string
  (** First tracker in the tier. *)
end
