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

(** Listener for incoming connections. *)

type t

type accept_fun = Lwt_unix.file_descr -> Unix.sockaddr -> unit

val create : accept_fun -> t
(** Create a new server to handle incoming connections.  For each accepted
    connection, the given function will be invoked with the corresponding file
    descriptor and the incoming node's address.  Must call {!start} to actually
    start accepting connections. *)
  
val start : t -> ?port:int -> unit -> unit
(** Start the accept loop.  If the optional argument is omitted, a random port
    is used. *)
  
val stop : t -> unit
(** Stop the accept loop. *)
  
val port : t -> int
(** The port on which we are listening for incoming connections.  Fails if the
    server is not running. *)
