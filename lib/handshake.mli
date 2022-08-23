type result = Ok of Util.socket * Bits.t * SHA1.t | Failed

type addr = Unix.inet_addr * int

val outgoing : id:SHA1.t -> info_hash:SHA1.t -> addr -> (result -> unit) -> unit
