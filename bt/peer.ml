(* The MIT License (MIT)

   Copyright (c) 2014-2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type sha1 = SHA1.t
type bitfield = Bitfield.t

type wire =
  { on_handshake : (sha1 * sha1 * extension list) Lwt_condition.t;
    on_choke : unit Lwt_condition.t;
    on_unchoke : unit Lwt_condition.t;
    on_interested : unit Lwt_condition.t;
    on_uninterested : unit Lwt_condition.t;
    on_bitfield : bitfield Lwt_condition.t;
    on_extended : (int * string) Lwt_condition.t;
    mutable peer_choking : bool;
    mutable am_choking : bool;
    mutable peer_interested : bool;
    mutable am_interested : bool;
    mutable has : bitfield;
    mutable uploaded : int64;
    mutable downloaded : int64;
    mutable finished : bool }

let handle_choke w =
  w.set_peer_choking true;
  Lwt_sequence.iter_node_l (fun n ->
    let _, u = Lwt_sequence.get n in
    Lwt_sequence.remove n;
    Lwt.wakeup_exn u Peer_choking) w.requests

let handle_unchoke w =
  w.set_peer_choking false

let handle_interested w =
  w.set_peer_interested true

let handle_uninterested w =
  w.set_peer_interested false

let handle_handshake w ih id e =
  w.exts <- e;
  w.fire_handshake (ih, id, e)

let handle_bitfield w bits =
  let get_bit b i =
    let j = i / 8 and k = i mod 8 in
    let b = BE.get_uint8 b j in
    b land (1 lsl k) <> 0
  in
  let rec loop i =
    if i < 8 * Lwt_bytes.length bits then
      (w.peer_pieces.(i) <- get_bit bits i; loop (i + 1))
  in
  loop 0;
  w.fire_bitfield ()

let handle_request w b =
  if w.am_choking then invalid_arg "choked peer";
  let t, u = Lwt.wait () in
  Lwt_sequence.add_r (b, u) w.peer_requests;
  t

let pull seq (i, o, l) =
  try
    let n =
      Lwt_sequence.find_node_l
        (fun ((i', o', l'), _) -> i = i' && o = o' && l = l') seq
    in
    let _, u = Lwt_sequence.get n in
    Lwt_sequence.remove n;
    Some u
  with
  | Not_found -> None

let handle_piece w (index, offset, buffer) =
  match pull w.peer_requests (index, offset, Array1.length buffer) with
  | Some u ->
      Lwt.wakeup u buffer;
      w.fire_download (Lwt_bytes.length buffer)
  | None ->
      ()

let handle_port w p =
  w.set_port p

let handle_cancel w b =
  let _ = pull w.peer_requests b in
  w.fire_cancel b

let handle_extended w id data =
  w.fire_extended (id, data)

let handle_message w =
  match BE.get_uint8 p.buf 0 with
  | 0 -> handle_choke w
  | 1 -> handle_unchoke w
  | 2 -> handle_interested w
  | 3 -> handle_uinterested w


let choke w =
  if not (React.S.value w.am_choking) then begin
    w.set_am_choking true;
    Lwt_sequence.iter_node_l Lwt_sequence.remove w.peer_requests;
    push w Message_choke
  end

let unchoke w =
  if S.value w.am_choking then begin
    w.set_am_choking false;
    push w Message_unchoke
  end

let have w i =
  push w 4 i
