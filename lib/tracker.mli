module Event : sig
  type t = Started | Completed | Stopped
end

module Response : sig
  module Peer : sig
    type t = {
      ip : [ `Ipaddr of Unix.inet_addr | `Name of string ];
      port : int;
    }
  end

  type ok = { interval : int; peers : Peer.t list; peers6 : Peer.t list }
  type t = Failure of string | Ok of ok

  val to_sexp : t Sexp.Encoder.t
end

val announce :
  net:Eio.Net.t ->
  info_hash:string ->
  peer_id:string ->
  port:int ->
  uploaded:int ->
  downloaded:int ->
  left:int ->
  ?event:Event.t ->
  string ->
  Response.t
