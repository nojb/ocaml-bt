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

type 'a result =
  | Success of 'a * int
  | Failure

type 'a t =
  string -> int -> 'a result

let bind (p : 'a t) (f : 'a -> 'b t) : 'b t =
  fun s i ->
    match p s i with
    | Success (x, j) -> f x s j
    | Failure -> Failure

let return x =
  fun _ i -> Success (x, i)

let fail =
  fun _ _ -> Failure

let (>>=) = bind
let (>|=) p f = bind p (fun x -> return (f x))
let (>>) p q = bind p (fun _ -> q)
    
let alt p q =
  fun s i ->
    match p s i with | Failure -> q s i | Success _ as res -> res

let (>|) p x = bind p (fun _ -> return x)
let (<|>) p q = alt p q

let sat pred p =
  fun s i ->
    match p s i with
    | Success (x, j) -> if pred x then Success (x, j) else Failure
    | Failure -> Failure

let any_char =
  fun s i ->
    if i >= String.length s then Failure
    else Success (s.[i], i+1)

let char c =
  sat (fun ch -> ch = c) any_char

(* parses all the characters satisfying [pred] into
 * a string *)
let chars1_pred (pred : char -> bool) =
  fun s i ->
    let rec loop j =
      if j >= String.length s then
        if j > i then Success (String.sub s i (j-i), j)
        else Failure
      else
        if pred s.[j] then loop (j+1)
        else
          if j > i then Success (String.sub s i (j-i), j)
          else Failure
    in loop i

let digits =
  chars1_pred (fun ch -> '0' <= ch && ch <= '9')

let rec many p =
  many1 p <|> return []

and many1 p =
  p >>= fun x -> many p >|= fun xs -> x :: xs

let any_int =
  digits >|= int_of_string

let any_int64 =
  digits >|= Int64.of_string

let wrapped p1 p p2 =
  p1 >> p >>= fun res -> p2 >| res

let string_of_length len =
  fun s i ->
    if i+len > String.length s then Failure
    else Success (String.sub s i len, i+len)

let pair p1 p2 =
  p1 >>= fun x1 -> p2 >|= fun x2 -> (x1, x2)

let fix f =
  fun s i ->
    (f ()) s i

module type S = sig
  val uint8 : int t
  val sint8 : int t
  val uint16 : int t
  val sint16 : int t
  val int32 : int32 t
  val int : int t
  val int64 : int64 t
end

external swap16 : int -> int     = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external unsafe_get_u8  : string -> int -> int   = "%string_unsafe_get"
external unsafe_get_u16 : string -> int -> int   = "%caml_string_get16u"
external unsafe_get_32 : string -> int -> int32 = "%caml_string_get32u"
external unsafe_get_64 : string -> int -> int64 = "%caml_string_get64u"

let sign8 v =
  (v lsl (Sys.word_size-9)) asr (Sys.word_size-9)

let sign16 v =
  (v lsl (Sys.word_size-17)) asr (Sys.word_size-17)

module BE = struct
  let uint8 s i =
    if String.length s < i+1 then Failure
    else Success (unsafe_get_u8 s i, i+1)
  let sint8 s i =
    if String.length s < i+1 then Failure
    else Success (sign8 (unsafe_get_u8 s i), i+1)
  let uint16 s i =
    if String.length s < i+2 then Failure
    else if Sys.big_endian then Success (unsafe_get_u16 s i, i+2)
    else Success (swap16 (unsafe_get_u16 s i), i+2)
  let sint16 s i =
    if String.length s < i+2 then Failure
    else if Sys.big_endian then
      Success (sign16 (unsafe_get_u16 s i), i+2)
    else
      Success (sign16 (swap16 (unsafe_get_u16 s i)), i+2)
  let int32 s i =
    if String.length s < i+4 then Failure
    else if Sys.big_endian then Success (unsafe_get_32 s i, i+4)
    else Success (swap32 (unsafe_get_32 s i), i+4)
  let int =
    int32 >|= Int32.to_int
  let int64 s i =
    if String.length s < i+8 then Failure
    else if Sys.big_endian then Success (unsafe_get_64 s i, i+8)
    else Success (swap64 (unsafe_get_64 s i), i+8)
end

module LE = struct
  let uint8 s i =
    if String.length s < i+1 then Failure
    else Success (unsafe_get_u8 s i, i+1)
  let sint8 s i =
    if String.length s < i+1 then Failure
    else Success (sign8 (unsafe_get_u8 s i), i+1)
  let uint16 s i =
    if String.length s < i+2 then Failure
    else if Sys.big_endian then Success (swap16 (unsafe_get_u16 s i), i+2)
    else Success (unsafe_get_u16 s i, i+2)
  let sint16 s i =
    if String.length s < i+2 then Failure
    else if Sys.big_endian then Success (sign16 (swap16 (unsafe_get_u16 s i)), i+2)
    else Success (sign16 (unsafe_get_u16 s i), i+2)
  let int32 s i =
    if String.length s < i+4 then Failure
    else if Sys.big_endian then Success (swap32 (unsafe_get_32 s i), i+4)
    else Success (unsafe_get_32 s i, i+4)
  let int =
    int32 >|= Int32.to_int
  let int64 s i =
    if String.length s < i+8 then Failure
    else if Sys.big_endian then Success (swap64 (unsafe_get_64 s i), i+8)
    else Success (unsafe_get_64 s i, i+8)
end

let string s' =
  let l = String.length s' in
  fun s i ->
    if i+l > String.length s then
      Failure
    else
      let rec loop j =
        if j >= l then Success ((), i+l)
        else if s'.[j] = s.[i+j] then loop (j+1)
        else Failure
      in
      loop 0

let any_string s i =
  let len = String.length s in
  Success (String.sub s i (len-i), len)

let end_of_input s i =
  if i >= String.length s then Success ((), i)
  else Failure

let either p1 p2 s i =
  match p1 s i with
  | Success _ as res -> res
  | Failure -> p2 s i

exception Get_error

let run p s =
  match p s 0 with
  | Success (x, j) ->
    if j <> String.length s then raise Get_error
    else x
  | Failure ->
    raise Get_error

let run_partial p s =
  match p s 0 with
  | Success (x, j) ->
    x, j
  | Failure ->
    raise Get_error

let run_file p path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let s = String.create len in
  really_input ic s 0 len;
  close_in ic;
  run p s