let (>>=) = Lwt.(>>=)

class type socket =
  object
    method close : unit Lwt.t

    method read : string -> int -> int -> int Lwt.t
    method really_read : string -> int -> int -> unit Lwt.t
    method read_char : char Lwt.t
    method read_string : int -> string Lwt.t
    method read_int16 : int Lwt.t
    method read_int32 : int32 Lwt.t
        
    method write : string -> int -> int -> int Lwt.t
    method really_write : string -> int -> int -> unit Lwt.t
    method write_string : string -> unit Lwt.t
    method write_bitstring : Bitstring.bitstring -> unit Lwt.t
    method write_int16 : int -> unit Lwt.t
    method write_int32 : int32 -> unit Lwt.t

    method on_write : unit Lwt.t
  end

class virtual basic_socket =
  object (self)
    method virtual close : unit Lwt.t
    method virtual read : string -> int -> int -> int Lwt.t
    method really_read str pos len =
      let rec loop pos len =
        if len <= 0 then
          Lwt.return ()
        else
          self#read str pos len >>= function
          | 0 -> Lwt.fail End_of_file
          | n -> loop (pos + n) (len - n)
      in
      loop pos len
    method read_char =
      let buf = String.create 1 in
      self#really_read buf 0 1 >>= fun () ->
      Lwt.return buf.[0]
    method read_string len =
      let str = String.create len in
      self#really_read str 0 len >>= fun () ->
      Lwt.return str
    method read_int16 =
      self#read_string 2 >>= fun s ->
      bitmatch Bitstring.bitstring_of_string s with
      | { n : 16 } -> Lwt.return n
    method read_int32 =
      self#read_string 4 >>= fun s ->
      bitmatch Bitstring.bitstring_of_string s with
      | { n : 32 } -> Lwt.return n
    method virtual write : string -> int -> int -> int Lwt.t
    method private write0 str pos len =
      self#write str pos len >>= fun n ->
      self#did_write;
      Lwt.return n
    method really_write str pos len =
      let rec loop pos len =
        if len <= 0 then
          Lwt.return ()
        else
          self#write0 str pos len >>= fun n ->
          loop (pos + n) (len - n)
      in
      loop pos len
    method write_string str =
      self#really_write str 0 (String.length str)
    method write_bitstring (s, ofs, len) =
      assert (ofs land 7 = 0 && len land 7 = 0);
      self#really_write s (ofs lsr 3) (len lsr 3)
    method write_int16 n =
      self#write_bitstring (BITSTRING { n :  16 })
    method write_int32 n =
      self#write_bitstring (BITSTRING { n : 32 })
        
    val on_write_waiters : unit Lwt.u Lwt_sequence.t = Lwt_sequence.create ()

    method on_write =
      Lwt.add_task_r on_write_waiters
    method private did_write =
      if not (Lwt_sequence.is_empty on_write_waiters) then
        Lwt.wakeup (Lwt_sequence.take_l on_write_waiters) ()
  end

let of_fd fd : socket =
  object
    inherit basic_socket
    method read = Lwt_unix.read fd
    method write = Lwt_unix.write fd
    method close = Lwt_unix.close fd
  end

let connect addr =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect fd (Addr.to_sockaddr addr) >>= fun () ->
  Lwt.return (of_fd fd)

class type encrypted_socket =
  object
    inherit socket
    method enable_encryption : Cryptokit.Stream.stream_cipher -> unit
    method disable_encryption : unit
    method enable_decryption : Cryptokit.Stream.stream_cipher -> unit
    method disable_decryption : unit
  end

let encrypt sock =
  object
    inherit basic_socket
    val mutable enc = None
    val mutable dec = None
    method enable_encryption cipher = enc <- Some cipher
    method disable_encryption = enc <- None
    method enable_decryption cipher = dec <- Some cipher
    method disable_decryption = dec <- None        
    method read str pos len =
      match dec with
      | None ->
        sock#read str pos len
      | Some cipher ->
        let buf = String.create len in
        sock#read buf 0 len >>= fun n ->
        cipher#transform buf 0 str pos n;
        Lwt.return n
    method write str pos len =
      match enc with
      | None ->
        sock#write str pos len
      | Some cipher ->
        let buf = String.create len in
        cipher#transform str pos buf 0 len;
        sock#really_write buf 0 len >>= fun () ->
        Lwt.return len
    method close =
      begin match enc with Some enc -> enc#wipe | _ -> () end;
      begin match dec with Some dec -> dec#wipe | _ -> () end;
      sock#close
  end

let default_buffer_size = 32 * 1024
  
let out_channel ?(buffer_size = default_buffer_size) sock =
  let write_bytes b ofs len =
    let str = String.create len in
    Lwt_bytes.blit_bytes_string b ofs str 0 len;
    sock#really_write str 0 len >>= fun () ->
    Lwt.return len
  in
  Lwt_io.make ~buffer_size
    ~mode:Lwt_io.Output
    ~close:(fun () -> sock#close)
    write_bytes
                              
let in_channel ?(buffer_size = default_buffer_size) sock =
  let read_bytes b ofs len =
    let str = String.create len in
    sock#read str 0 len >>= fun n ->
    Lwt_bytes.blit_string_bytes str 0 b ofs n;
    Lwt.return n
  in
  Lwt_io.make ~buffer_size
    ~mode:Lwt_io.Input
    ~close:(fun () -> sock#close)
    read_bytes
