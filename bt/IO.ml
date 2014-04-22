let (>>=) = Lwt.(>>=)

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
    method really_write str pos len =
      let rec loop pos len =
        if len <= 0 then
          Lwt.return ()
        else
          self#write str pos len >>= fun n ->
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
  end

let of_fd fd =
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

let encrypt sock cipher =
  object
    inherit basic_socket
    method read str pos len =
      sock#read str pos len >>= fun n ->
      cipher#transform str pos str pos n;
      Lwt.return n
    method write str pos len =
      let buf = String.create len in
      cipher#transform str pos buf 0 len;
      sock#really_write buf 0 len >>= fun () ->
      Lwt.return len
    method close =
      cipher#wipe;
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
