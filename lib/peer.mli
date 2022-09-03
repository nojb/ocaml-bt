module Handshake : sig
  type t

  val to_sexp : t -> Sexp.t
end

type t

type error =
  [ `Connect_failed of [ `Exn of exn | `Timeout ]
  | `Handshake_failed of [ `Exn of exn | `Msg of string | `Timeout | `Info_hash_mismatch ]
  | `Expected_bitfield
  | `Msg of string ]

val string_of_error: error -> string

val run:
  net:Eio.Net.t ->
  clock:Eio.Time.clock ->
  sw:Eio.Switch.t ->
  info_hash:string ->
  peer_id:string -> Unix.inet_addr -> int -> (t, [> error]) result

val has_piece: t -> int -> bool
