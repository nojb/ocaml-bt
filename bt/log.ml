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

type section = string

let make_section name = name

let active = ref true

let render template section ?exn msg =
  let now = Unix.gettimeofday () in
  let msecs, now = modf now in
  let tm = Unix.localtime now in
  let b = Buffer.create 10 in
  Buffer.add_substitute b begin
    function
    | "message" ->
      msg
    | "date" ->
      Printf.sprintf "%04d-%02d-%02d"
        (tm.Unix.tm_year+1900) (tm.Unix.tm_mon+1) tm.Unix.tm_mday
    | "time" ->
      Printf.sprintf "%02d:%02d:%02d.%03d"
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (truncate (msecs *. 1000.0))
    | "section" ->
      section
    | "exn" ->
      begin match exn with
      | None -> ""
      | Some exn -> ": " ^ Printexc.to_string exn
      end
    | _ ->
      ""
  end template;
  Buffer.contents b

let debug section ?exn fmt =
  let template = "[$(date) $(time)] $(section): $(message)$(exn)" in
  Printf.ksprintf
    (fun msg -> if !active then prerr_endline (render template section ?exn msg))
    fmt
