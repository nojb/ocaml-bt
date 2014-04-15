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

type 'a t

val string : string -> unit t
val any_string : string t
val end_of_input : unit t
val either : 'a t -> 'a t -> 'a t
    
val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
val return : 'a -> 'a t
val fail : 'a t

val alt : 'a t -> 'a t -> 'a t
val (>>) : 'a t -> 'b t -> 'b t
val (>|) : 'a t -> 'b -> 'b t
val (<|>) : 'a t -> 'a t -> 'a t

val sat : ('a -> bool) -> 'a t -> 'a t
val any_char : char t
val char : char -> char t
val chars1_pred : (char -> bool) -> string t
val digits : string t
val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t
val any_int : int t
val any_int64 : int64 t
val wrapped : 'a t -> 'b t -> 'a t -> 'b t
val string_of_length : int -> string t
val pair : 'a t -> 'b t -> ('a * 'b) t
val fix : (unit -> 'a t) -> 'a t

exception Get_error

val run : 'a t -> string -> 'a
val run_partial : 'a t -> string -> 'a * int
val run_file : 'a t -> string -> 'a
