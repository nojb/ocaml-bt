type t = { xt : SHA1.t; dn : string option; tr : Uri.t list }

val parse : string -> [ `Ok of t | `Error ]
(** Parses a BitTorrent magnet link. *)
