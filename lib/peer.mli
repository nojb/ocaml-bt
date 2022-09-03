module Handshake : sig
  type t

  val to_sexp : t -> Sexp.t
end

module Message : sig
  type t =
    | Keepalive
    | Choke
    | Unchoke
    | Interested
    | Not_interested
    | Have of int
    | Bitfield of string
    | Request of { i : int; ofs : int; len : int }
    | Piece of { i : int; ofs : int; data : string }
    | Cancel of { i : int; ofs : int; len : int }

  val to_sexp : t -> Sexp.t
end

type t

type error =
  [ `Connect_failed of [ `Exn of exn | `Timeout ]
  | `Handshake_failed of
    [ `Exn of exn | `Msg of string | `Timeout | `Info_hash_mismatch ]
  | `Expected_bitfield
  | `Msg of string ]

val string_of_error : error -> string

val run :
  net:Eio.Net.t ->
  clock:Eio.Time.clock ->
  sw:Eio.Switch.t ->
  info_hash:string ->
  peer_id:string ->
  Unix.inet_addr ->
  int ->
  (t, [> error ]) result

val choked : t -> bool
val has_piece : t -> int -> bool
val send_unchoke : t -> unit
val send_interested : t -> unit
val send_request : t -> i:int -> ofs:int -> len:int -> unit
val read_message : t -> Message.t
