type t

val close : t -> unit Lwt.t
val read : t -> string -> int -> int -> int Lwt.t
val write : t -> string -> int -> int -> int Lwt.t
val really_read : t -> string -> int -> int -> unit Lwt.t
val read_char : t -> char Lwt.t
val read_string : t -> int -> string Lwt.t
val read_int16 : t -> int Lwt.t
val read_int32 : t -> int32 Lwt.t
val really_write : t -> string -> int -> int -> unit Lwt.t
val write_string : t -> string -> unit Lwt.t
val write_bitstring : t -> Bitstring.bitstring -> unit Lwt.t
val write_int16 : t -> int -> unit Lwt.t
val write_int32 : t -> int32 -> unit Lwt.t
val on_write : t -> unit Lwt.t
val enable_encryption : t -> Cryptokit.Stream.stream_cipher -> unit
val disable_encryption : t -> unit
val enable_decryption : t -> Cryptokit.Stream.stream_cipher -> unit
val disable_decryption : t -> unit

(* class type socket = *)
(*   object *)
(*     method close : unit Lwt.t *)

(*     method read : string -> int -> int -> int Lwt.t *)
(*     method really_read : string -> int -> int -> unit Lwt.t *)
(*     method read_char : char Lwt.t *)
(*     method read_string : int -> string Lwt.t *)
(*     method read_int16 : int Lwt.t *)
(*     method read_int32 : int32 Lwt.t *)
        
(*     method write : string -> int -> int -> int Lwt.t *)
(*     method really_write : string -> int -> int -> unit Lwt.t *)
(*     method write_string : string -> unit Lwt.t *)
(*     method write_bitstring : Bitstring.bitstring -> unit Lwt.t *)
(*     method write_int16 : int -> unit Lwt.t *)
(*     method write_int32 : int32 -> unit Lwt.t *)

(*     method on_write : unit Lwt.t *)

(*     method enable_encryption : Cryptokit.Stream.stream_cipher -> unit *)
(*     method disable_encryption : unit *)
(*     method enable_decryption : Cryptokit.Stream.stream_cipher -> unit *)
(*     method disable_decryption : unit *)
(*   end *)

val create : Addr.t -> t

val connect : t -> unit Lwt.t
val reconnect : t -> unit Lwt.t

val addr : t -> Addr.t

(* val of_fd : Lwt_unix.file_descr -> t *)

(* class type encrypted_socket = *)
(*   object *)
(*     inherit socket *)
(*   end *)

(* val encrypt : socket -> encrypted_socket *)
    (* read:Cryptokit.Stream.stream_cipher -> write:Cryptokit.Stream.stream_cipher -> socket *)

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

val out_channel : ?buffer_size:int -> t -> Lwt_io.output_channel
val in_channel : ?buffer_size:int -> t -> Lwt_io.input_channel
