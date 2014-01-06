(** based on
 * http://stackoverflow.com/questions/3758606/how-to-convert-byte-size-into-human-readable-format-in-java *)
let string_of_file_size (b : int64) : string =
  let step = 1024.0 in
  let round d = floor (d +. 0.5) in
  let b' = Int64.to_float b in
  if b' < step then Printf.sprintf "%Ld B" b
  else
    let exp = int_of_float (log b' /. log step) in
    let b'' = round (10.0 *. b' /. step ** (float exp)) /. 10.0 in
    Printf.sprintf "%g %ciB" b'' ("KMGTPE".[exp-1])
(*     Printf.sprintf "%.1f %ciB" *)
(*       (b' /. step ** (float exp)) ("KMGTPE".[exp-1]) *)

let string_of_sockaddr = function
  | Unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port
  | Unix.ADDR_UNIX name ->
    name

let read_exactly fd n =
  let (>>=) = Lwt.(>>=) in
  let s = String.create n in
  let rec loop o l =
    if l <= 0 then
      Lwt.return s
    else
      Lwt_unix.read fd s o l >>= fun l' ->
      if l' = 0 then Lwt.fail End_of_file
      else loop (o+l') (l-l')
  in
  loop 0 (String.length s)

let write_fully fd s =
  let (>>=) = Lwt.(>>=) in
  let rec loop o l =
    if l <= 0 then
      Lwt.return_unit
    else
      Lwt_unix.write fd s o l >>= fun l' ->
      assert (l' > 0);
      loop (o+l') (l-l')
  in
  loop 0 (String.length s)
