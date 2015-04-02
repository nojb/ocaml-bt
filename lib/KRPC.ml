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

(* let section = Log.make_section "KRPC" *)

(* let debug ?exn fmt = Log.debug section ?exn fmt *)

let (>>=) = Lwt.(>>=)

open Printf

let alpha = 3

type msg =
  | Query of string * (string * Bcode.t) list
  | Response of (string * Bcode.t) list
  | Error of int64 * string

let string_of_msg = function
  | Query (name, args) -> sprintf "query %s(%s)" name (String.concat "," (List.map fst args))
  | Response d -> sprintf "response (%s)" (String.concat "," (List.map fst d))
  | Error (n, s) -> sprintf "error (%Ld,%S)" n s

let encode t msg =
  let str s = Bcode.String (Cstruct.of_string s) in
  let d = match msg with
    | Query (name, args) ->
      ["y", str "q"; "q", str name; "a", Bcode.Dict args]
    | Response dict ->
      ["y", str "r"; "r", Bcode.Dict dict]
    | Error (code, s) ->
      ["y", str "e"; "e", Bcode.List [Bcode.Int code; str s]]
  in
  let d = ("t", str t) :: d in
  Bcode.encode (Bcode.Dict d)

let decode s =
  let bc = Bcode.decode s in
  let t = Bcode.to_string (Bcode.find "t" bc) in
  let y = Bcode.to_string (Bcode.find "y" bc) in
  let msg = match y with
    | "q" ->
      let q = Bcode.to_string (Bcode.find "q" bc) in
      let a = Bcode.to_dict (Bcode.find "a" bc) in
      Query (q, a)
    | "r" ->
      let r = Bcode.to_dict (Bcode.find "r" bc) in
      Response r
    | "e" ->
      begin match Bcode.to_list (Bcode.find "e" bc) with
      | Bcode.Int n :: Bcode.String s :: _ -> Error (n, Cstruct.to_string s)
      | _ -> failwith "KRPC.decode: bad fields for 'e' entry"
      end
    | _ ->
      failwith (Printf.sprintf "KRPC.decode: unknown message type y %S" y)
  in
  (t, msg)

module Assoc2 : sig
  type ('a, 'b, 'c) t
  val create : unit -> ('a, 'b, 'c) t
  val add : ('a, 'b, 'c) t -> 'a -> 'b -> 'c -> unit
  val find : ('a, 'b, 'c) t -> 'a -> 'b -> 'c option
  val remove : ('a, 'b, 'c) t -> 'a -> 'b -> unit
  val clear : ('a, 'b, 'c) t -> unit
  val iter : ('a -> 'b -> 'c -> unit) -> ('a, 'b, 'c) t -> unit
end = struct
  type ('a, 'b, 'c) t = ('a, ('b, 'c) Hashtbl.t) Hashtbl.t
  let create () = Hashtbl.create 3
  let add h a b c =
    let hh = try Hashtbl.find h a with Not_found -> Hashtbl.create 3 in
    Hashtbl.add hh b c;
    Hashtbl.replace h a hh
  let find h a b =
    try Some (Hashtbl.find (Hashtbl.find h a) b) with Not_found -> None
  let remove h a b =
    try Hashtbl.remove (Hashtbl.find h a) b with Not_found -> ()
  let clear h =
    Hashtbl.clear h
  let iter f h =
    Hashtbl.iter (fun a h -> Hashtbl.iter (fun b c -> f a b c) h) h
end

type addr = Unix.inet_addr * int

type answer_func = addr -> string -> (string * Bcode.t) list -> msg

type rpc =
  | Error
  | Timeout
  | Response of addr * (string * Bcode.t) list

type t = {
  sock : UDP.socket;
  answer : answer_func;
  pending : (addr, string, rpc Lwt.u * float) Assoc2.t
}

let create answer port =
  { sock = UDP.create_socket ~port (); answer; pending = Assoc2.create () }

let read_one_packet krpc =
  UDP.recv krpc.sock >>= fun (s, addr) ->
  let (t, msg) = decode (Cstruct.of_string s) in
  match msg with
  | Error (code, msg) ->
    begin match Assoc2.find krpc.pending addr t with
    | None ->
        ()
      (* debug "no t:%S for %s" t (addro_string addr) *)
    | Some (w, _) ->
      Assoc2.remove krpc.pending addr t;
      Lwt.wakeup w Error
    end;
    Lwt.return ()
  | Query (name, args) ->
    let ret = krpc.answer addr name args in
    UDP.send krpc.sock (Cstruct.to_string (encode t ret)) addr
  | Response args ->
    begin match Assoc2.find krpc.pending addr t with
    | None ->
      (* debug "no t:%S for %s" t (addro_string addr); *)
      Lwt.return ()
    | Some (w, _) ->
      Assoc2.remove krpc.pending addr t;
      Lwt.wakeup w (Response (addr, args));
      Lwt.return ()
    end

let rec read_loop krpc =
  read_one_packet krpc >>= fun () -> read_loop krpc

let fresh_txn =
  let last = ref 0 in
  fun () ->
    let id = string_of_int !last in
    incr last;
    id

let timeout_check_delay = 5.0

let rec timeout_pulse krpc =
  let now = Unix.time () in
  let bad = ref [] in
  Assoc2.iter
    (fun addr t (w, at_most) -> if at_most < now then bad := (addr, t, w) :: !bad)
    krpc.pending;
  List.iter
    (fun (addr, t, w) -> Assoc2.remove krpc.pending addr t; Lwt.wakeup w Timeout)
    !bad;
  Lwt_unix.sleep timeout_check_delay >>= fun () -> timeout_pulse krpc

let start krpc =
  Lwt.async (fun () -> read_loop krpc);
  Lwt.async (fun () -> timeout_pulse krpc)

let timeout_delay = 20.0

let send_msg krpc msg addr =
  let t = fresh_txn () in (* FIXME only outstanding queries need be unique *)
  let wait, w = Lwt.wait () in
  Assoc2.add krpc.pending addr t (w, Unix.time () +. timeout_delay);
  UDP.send krpc.sock (Cstruct.to_string (encode t msg)) addr >>= fun () -> wait
