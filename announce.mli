type t

val create :
  Word160.t ->
  Uri.t list list ->
  (unit -> int64) ->
  (unit -> int64) ->
  (unit -> int64) ->
  int ->
  Word160.t ->
  (Unix.sockaddr -> unit) ->
  t

val start : t -> unit
val stop : t -> unit
