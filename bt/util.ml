(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

let string_of_sockaddr = function
  | Unix.ADDR_INET (a, p) -> Printf.sprintf "%s:%u" (Unix.string_of_inet_addr a) p
  | Unix.ADDR_UNIX s -> s


module W = struct
  type t = int * (Cstruct.t -> int -> unit)

  let len (l, _) = l
  let empty = (0, fun _ _ -> ())
  let append (l1, f1) (l2, f2) = (l1 + l2, fun cs o -> f1 cs o; f2 cs (o + l1))
  let (<+>) = append
  let char c = (1, fun cs o -> Cstruct.set_uint8 cs o (Char.code c))
  let byte b = (1, fun cs o -> Cstruct.set_uint8 cs o b)
  let string s = (String.length s, fun cs o -> Cstruct.blit_from_string s 0 cs o (String.length s))
  let int32 n = (4, fun cs o -> Cstruct.BE.set_uint32 cs o n)
  let int n = (4, fun cs o -> Cstruct.BE.set_uint32 cs o (Int32.of_int n))
  let int16 n = (2, fun cs o -> Cstruct.BE.set_uint16 cs o n)
  let concat l = List.fold_left append empty l
  let immediate x = (Cstruct.len x, fun cs o -> Cstruct.blit x 0 cs o (Cstruct.len x))

  let to_cstruct (l, f) =
    let cs = Cstruct.create l in
    f cs 0;
    cs
end
