type t

type event =
  | NOTHING

val create : Word160.t -> Uri.t list -> (event -> unit) -> t
