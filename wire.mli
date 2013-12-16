type block =
  | Block of int * int

type msg =
  | KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have of int
  | BitField of Bits.t
  | Request of int * block
  | Piece of int * int * string
  | Cancel of int * block
  | Port of int
  | Extended of int * string

(* val size_of_msg : msg -> int *)

exception BadMsg of int * int

val string_of_msg : msg -> string
val read : Lwt_io.input_channel -> msg Lwt.t
val write : Lwt_io.output_channel -> msg -> int Lwt.t
