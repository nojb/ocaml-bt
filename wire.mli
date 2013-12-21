type block =
  | Block of int * int * int

type msg =
  | KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have of int
  | BitField of Bits.t
  | Request of block
  | Piece of int * int * string
  | Cancel of block
  | Port of int
  | Extended of int * string

exception BadMsg of int * int

val string_of_msg : msg -> string
val get : int -> msg Get.t
val put : msg -> Put.t
val read : Lwt_io.input_channel -> msg Lwt.t
val write : Lwt_io.output_channel -> msg -> unit Lwt.t
