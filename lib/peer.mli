module Handshake : sig
  type t

  val to_sexp : t -> Sexp.t
end

type t

type error =
  [ `Connect_failed
  | `Connect_timeout
  | `Info_hash_mismatch
  | `Handshake_failed of [ `Exn of exn | `Msg of string | `Timeout ] ]

val string_of_error: error -> string

val run: net:Eio.Net.t -> clock:Eio.Time.clock -> info_hash:string -> peer_id:string -> Unix.inet_addr -> int -> (Handshake.t, [> error]) result
