let (>>=) = Lwt.(>>=)
              
module Ip = struct
  type t = Unix.inet_addr

  let any = Unix.inet_addr_any

  let loopback = Unix.inet_addr_loopback

  let of_string s =
    let he = Unix.gethostbyname s in
    he.Unix.h_addr_list.(0)

  let of_string_noblock s =
    Lwt_unix.gethostbyname s >>= fun he ->
    Lwt.return he.Unix.h_addr_list.(0)

  let to_string ip =
    Unix.string_of_inet_addr ip

  let of_ints a b c d =
    Unix.inet_addr_of_string (Printf.sprintf "%d.%d.%d.%d" a b c d)

  let to_ints ip =
    Scanf.sscanf (Unix.string_of_inet_addr ip) "%d.%d.%d.%d" (fun a b c d -> (a, b, c, d))
end

type t = Ip.t * int

let port (_, p) = p

let ip (ip, _) = ip

let to_string (ip, p) = Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) p

let to_string_compact (ip, p) =
  let (a, b, c, d) = Ip.to_ints ip in
  let pl, ph = p land 0xff, (p land 0xff00) lsr 8 in
  Printf.sprintf "%c%c%c%c%c%c"
    (Char.chr a) (Char.chr b) (Char.chr c) (Char.chr d) (Char.chr ph) (Char.chr pl)

let to_sockaddr (ip, p) =
  Unix.ADDR_INET (ip, p)

let of_sockaddr = function
  | Unix.ADDR_UNIX _ -> failwith "of_sockaddr"
  | Unix.ADDR_INET (ip, p) -> (ip, p)
