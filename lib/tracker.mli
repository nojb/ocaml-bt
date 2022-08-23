module Event : sig
  type t = Started | Completed | Stopped
end

module Response : sig
  module Peer : sig
    type t = { id : string; ip : string; port : int }
  end

  type ok = { interval : int; peers : Peer.t list }

  type t = Failure of string | Ok of ok

  val to_sexp : t Sexp.Encoder.t
end

val announce :
  info_hash:string ->
  peer_id:string ->
  port:int ->
  uploaded:int ->
  downloaded:int ->
  left:int ->
  ?event:Event.t ->
  url:string ->
  Response.t
