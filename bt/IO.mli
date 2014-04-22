class type socket =
  object
    method close : unit Lwt.t

    method read : string -> int -> int -> int Lwt.t
    method really_read : string -> int -> int -> unit Lwt.t
    method read_string : int -> string Lwt.t
    method read_int16 : int Lwt.t
    method read_int32 : int32 Lwt.t
        
    method write : string -> int -> int -> int Lwt.t
    method really_write : string -> int -> int -> unit Lwt.t
    method write_string : string -> unit Lwt.t
    method write_bitstring : Bitstring.bitstring -> unit Lwt.t
    method write_int16 : int -> unit Lwt.t
    method write_int32 : int32 -> unit Lwt.t
  end

val connect : Addr.t -> socket Lwt.t

val of_fd : Lwt_unix.file_descr -> socket

val encrypt : socket -> Cryptokit.Stream.stream_cipher -> socket

(* class type buffered_output = *)
(*   object *)
(*     method flush : unit *)
(*     method write : string -> int -> int -> int Lwt.t *)
(*     method really_write : string -> int -> int -> unit Lwt.t *)
(*     method write_char : char -> unit Lwt.t *)
(*     method write_string : string -> unit Lwt.t *)
(*     method write_bitstring : Bitstring.bitstring -> unit Lwt.t *)
(*     method write_int : int -> unit Lwt.t *)
(*     method write_int16 : int -> unit Lwt.t *)
(*     method write_int32 : int32 -> unit Lwt.t *)
(*   end *)

(* class type buffered_input = *)
(*   object *)
(*     method read_char : char *)
(*     method read_string : int -> string Lwt.t *)
(*     method read : string -> int -> int -> int Lwt.t *)
(*     method really_read : string -> int -> int -> unit Lwt.t *)
(*     method read_int : int *)
(*     method read_int16 : int *)
(*     method read_int32 : int32 *)
(*   end *)
  
(* val buffered_input : ?buffer_size:int -> socket -> buffered_input *)
                              
(* val buffered_output : ?buffer_size:int -> socket -> buffered_output *)

val out_channel : ?buffer_size:int -> socket -> Lwt_io.output_channel
val in_channel : ?buffer_size:int -> socket -> Lwt_io.input_channel
