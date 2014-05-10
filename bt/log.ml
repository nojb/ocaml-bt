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

let now () = Unix.gettimeofday ()
    
let start_time = ref (now ())

let reset_timer () = start_time := now ()

type level = 
 | DEBUG
 | INFO
 | NOTICE
 | WARNING
 | ERROR
 | FATAL

let current_level = ref FATAL

type color =
  | NONE
  | RED
  | GREEN
  | YELLOW
  | BLUE
  | MAGENTA
  | CYAN
  | WHITE

let colorcode = function
  | NONE -> ""
  | RED -> "\027[31m"
  | GREEN -> "\027[32m"
  | YELLOW -> "\027[33m"
  | BLUE -> "\027[34m"
  | MAGENTA -> "\027[35m"
  | CYAN -> "\027[36m"
  | WHITE -> "\027[37m"

let string_of_level = function
  | DEBUG -> "debug"
  | INFO -> "info"
  | NOTICE -> "notice"
  | WARNING -> "warning"
  | ERROR -> "error"
  | FATAL -> "fatal"

let color_of_level = function
  | DEBUG -> CYAN
  | INFO -> WHITE
  | NOTICE -> GREEN
  | WARNING -> YELLOW
  | ERROR -> RED
  | FATAL -> RED

let log level ?exn fmt =
  let title = string_of_level level in
  let color = if Unix.isatty Unix.stderr then color_of_level level else NONE in
  if level >= !current_level then
    Format.kfprintf (fun fmt ->
        Printf.eprintf "\027[34m[%.3f]\027[0m %s%s\027[0m %s%s\n%!"
          (now () -. !start_time)
          (colorcode color)
          ("(" ^ title ^ ")")
          (Format.flush_str_formatter ())
          (match exn with None -> "" | Some exn -> ": " ^ (Printexc.to_string exn)))
      Format.str_formatter fmt
  else
    Format.ikfprintf (fun _ -> ()) Format.str_formatter fmt
    
let error ?exn fmt = log ERROR ?exn fmt

let info ?exn fmt = log INFO ?exn fmt

let warning ?exn fmt = log WARNING ?exn fmt

let success ?exn fmt = log NOTICE ?exn fmt
    
let debug ?exn fmt = log DEBUG ?exn fmt
