type level = 
 | DEBUG
 | INFO
 | NOTICE
 | WARNING
 | ERROR

val current_level : level ref

val reset_timer : unit -> unit

val info : ?exn:exn -> ('a, Format.formatter, unit, unit) format4 -> 'a
val error : ?exn:exn -> ('a, Format.formatter, unit, unit) format4 -> 'a
val warning : ?exn:exn -> ('a, Format.formatter, unit, unit) format4 -> 'a
val success : ?exn:exn -> ('a, Format.formatter, unit, unit) format4 -> 'a
val debug : ?exn:exn -> ('a, Format.formatter, unit, unit) format4 -> 'a
