val reset_timer : unit -> unit

val info : ?sec:string -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
val error : ?sec:string -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
val warning : ?sec:string -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
val success : ?sec:string -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
