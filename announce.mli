type t

val create :
  Info.t ->
  (unit -> int64) ->
  (unit -> int64) ->
  (unit -> int64) ->
  int ->
  Word160.t ->
  (Unix.sockaddr -> unit) ->
  t

val start : t -> unit
val stop : t -> unit
