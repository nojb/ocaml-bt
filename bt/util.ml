(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

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

let really_read fd buf off len =
  let (>>=) = Lwt.(>>=) in
  assert (off + len <= String.length buf);
  let rec loop off len =
    if len <= 0 then
      Lwt.return ()
    else
      Lwt_unix.read fd buf off len >>= fun len' ->
      if len' = 0 then Lwt.fail End_of_file
      else loop (off + len') (len - len')
  in
  loop off len

let read_exactly fd n =
  let (>>=) = Lwt.(>>=) in
  let s = String.create n in
  really_read fd s 0 n >>= fun () ->
  Lwt.return s

let really_write fd buf off len =
  let (>>=) = Lwt.(>>=) in
  assert (off + len <= String.length buf);
  let rec loop off len =
    if len <= 0 then
      Lwt.return ()
    else
      Lwt_unix.write fd buf off len >>= fun len' ->
      assert (len' > 0);
      loop (off + len') (len - len')
  in
  loop off len

let write_fully fd s =
  really_write fd s 0 (String.length s)

let string_of_sockaddr = function
  | Unix.ADDR_INET (a, p) -> Printf.sprintf "%s:%u" (Unix.string_of_inet_addr a) p
  | Unix.ADDR_UNIX s -> s

let shuffle_array a =
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i+1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done

let array_exists f a =
  let rec loop i =
    if i >= Array.length a then false else
    if f a.(i) then true
    else loop (i+1)
  in
  loop 0

let safe_int64_to_int n =
  let m = Int64.to_int n in
  assert (Int64.(compare (of_int m) n) = 0);
  m
