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

let debug ?exn fmt = Log.debug (Log.make_section "Listener") ?exn fmt

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type running = {
  sock : Lwt_unix.file_descr;
  on_stop : unit Lwt_condition.t;
  port : int
}

type stage =
  | Running of running
  | Stopped

type t = {
  mutable stage : stage;
  on_accept : Lwt_unix.file_descr -> Unix.sockaddr -> unit
}

type accept_fun = Lwt_unix.file_descr -> Unix.sockaddr -> unit

let create on_accept =
  { stage = Stopped; on_accept }

let accept_loop sock on_stop on_accept =
  debug "starting";
  let rec loop () =
    Lwt.pick
      [(Lwt_unix.accept sock >|= fun (fd, sa) -> `Accepted (fd, sa));
       (Lwt_condition.wait on_stop >|= fun () -> `Stop)]
    >>= function
    | `Accepted (fd, sa) ->
      debug "accepted connection from %s" (Util.string_of_sockaddr sa);
      on_accept fd sa;
      loop ()
    | `Stop ->
      debug "stopping";
      Lwt.return ()
  in
  loop ()

let listen_backlog = 5

let start lstnr ?port () =
  match lstnr.stage with
  | Running _ -> ()
  | Stopped ->
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let on_stop = Lwt_condition.create () in
    let port = match port with Some p -> p | None -> Random.int 65536 in
    Lwt_unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Lwt_unix.listen sock listen_backlog;
    debug "listening on port %u" port;
    Lwt.async (fun () -> accept_loop sock on_stop lstnr.on_accept);
    lstnr.stage <- Running { sock; on_stop; port }

let stop lstnr =
  match lstnr.stage with
  | Stopped -> ()
  | Running r -> Lwt_condition.broadcast r.on_stop ()

let port lstnr =
  match lstnr.stage with
  | Stopped -> None
  | Running r -> Some r.port
