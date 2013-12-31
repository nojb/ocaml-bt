type t = {
  xt : Word160.t;
  dn : string option;
  tr : Uri.t list
}

(** Parses a BitTorrent magnet link.
    Raises Invalid_argument "Magnet.of_string" if parsing fails for any reason *)
val of_string : string -> t
