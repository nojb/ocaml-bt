type message =
  | KEEP_ALIVE
  | CHOKE
  | UNCHOKE
  | INTERESTED
  | NOT_INTERESTED
  | HAVE of int
  | BITFIELD of Bits.t
  | REQUEST of int * int * int
  | PIECE of int * int * string
  | CANCEL of int * int * int
  | PORT of int
  | HAVE_ALL
  | HAVE_NONE
  | SUGGEST of int
  | REJECT of int * int * int
  | ALLOWED of int list
  | EXTENDED of int * string

exception BadMsg of int * int

val string_of_message : message -> string
val sprint : message -> unit -> string
val get : int -> message Get.t
val put : message -> Put.t
val read : Lwt_unix.file_descr -> message Lwt.t
(* val write : Lwt_io.output_channel -> message -> unit Lwt.t *)
