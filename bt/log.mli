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

(** Simple Log system. Messages are printed out only if their level (see
    {!level}) is higher than the value of {!log_level}. *)

type section
(** Sections.  These are printed as headers in the log message. *)

(** Log levels *)
type level =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Fatal

val make_section : string -> section
(** Make a section with the given name.  The name will appear in the header of
    the log message. *)

val log_level : level ref
(** The current log level.  Messages with a level at least this will get printed
    on [stderr]. *)

val debug : section -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
(** Log a debug message. *)
  
val info : section -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
(** Log an informational message. *)
  
val notice : section -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
(** Log an informational message. *)
  
val warning : section -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
(** Log a warning. *)
  
val error : section -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
(** Log an error. *)
  
val fatal : section -> ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
(** Log a fatal message. *)
