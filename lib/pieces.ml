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

let block_size = 1 lsl 14 (* 16384 *)
let max_requests = 5

open Lwt.Infix

type piece_state =
  | Pending
  | Active of int array
  | Verified

type piece =
  { mutable state : piece_state;
    length : int;
    offset : int64;
    mutable have : int;
    hash : SHA1.t }

let request_block parts = function
  | false ->
      let rec loop i =
        if i >= Array.length parts then
          None
        else
        if parts.(i) = 0 then
          (parts.(i) <- 1; Some i)
        else
          loop (i + 1)
      in
      loop 0
  | true ->
      let rec loop min index i =
        if i >= Array.length parts then
          if index >= 0 then
            (parts.(index) <- parts.(index) + 1; Some index)
          else
            None
        else
        if parts.(i) >= 0 && (parts.(i) < min || min < 0) then
          loop parts.(i) i (i + 1)
        else
          loop min index (i + 1)
      in
      loop (-1) (-1) 0

let request_endgame pieces has =
  let rec loop i =
    if i >= Array.length pieces then
      None
    else
      match pieces.(i).state with
      | Verified
      | Pending ->
          loop (i + 1)
      | Active parts ->
          if has i then
            match request_block parts true with
            | Some j ->
                Log.debug "Requesting ENDGAME block #%d of #%d" j i;
                Some (i, j)
            | None ->
                None
          else
            loop (i + 1)
  in
  loop 0

let request_pending pieces has start finish =
  if start < 0 || start > finish || finish > Array.length pieces then
    invalid_arg "request_pending";
  let rec loop i =
    if i >= finish then
      None
    else
      match pieces.(i).state with
      | Pending ->
          let nparts = (pieces.(i).length + block_size - 1) / block_size in
          let parts = Array.make nparts 0 in
          pieces.(i).state <- Active parts;
          begin match request_block parts false with
          | None ->
              assert false
          | Some j ->
              Log.debug "Requesting PENDING block #%d of #%d" j i;
              Some (i, j)
          end
      | Active _
      | Verified ->
          loop (i + 1)
  in
  loop start

let request_pending pieces has =
  let n = Random.int (Array.length pieces + 1) in
  match request_pending pieces has n (Array.length pieces) with
  | None ->
      begin match request_pending pieces has 0 n with
      | None ->
          request_endgame pieces has
      | Some _ as r ->
          r
      end
  | Some _ as r ->
      r

let request_active pieces has =
  let rec loop i =
    if i >= Array.length pieces then
      request_pending pieces has
    else
      match pieces.(i).state with
      | Verified
      | Pending ->
          loop (i + 1)
      | Active parts ->
          if has i then
            match request_block parts false with
            | None ->
                loop (i + 1)
            | Some j ->
                Log.debug "Requesting ACTIVE block #%d of #%d" j i;
                Some (i, j)
          else
            loop (i + 1)
  in
  loop 0

let request p pieces =
  if Peer.requests p < max_requests then
    match request_active pieces (Peer.has p) with
    | Some (i, j) ->
        let off = j * block_size in
        let len = min block_size (pieces.(i).length - off) in
        Peer.request p i off len
    | None ->
        ()

let update_requests pieces peers =
  Hashtbl.iter (fun _ p -> if not (Peer.peer_choking p) then request p pieces) peers

let update_interest pieces p =
  let rec loop i =
    if i < Array.length pieces then
      if Peer.has p i then
        match pieces.(i).state with
        | Active _
        | Pending ->
            Peer.interested p
        | Verified ->
            loop (i + 1)
      else
        loop (i + 1)
    else
      Peer.not_interested p
  in
  loop 0

let send_block store pieces p i off len =
  match pieces.(i).state with
  | Verified ->
      let off1 = Int64.(add pieces.(i).offset (of_int off)) in
      Lwt.ignore_result (Store.read store off1 len >>= Lwt.wrap4 Peer.piece p i off)
  | Pending
  | Active _ ->
      ()

let verify_piece store peers pieces i k =
  Store.digest store pieces.(i).offset pieces.(i).length begin function
  | `Ok sha ->
      k (if SHA1.equal sha pieces.(i).hash then `Ok () else `Error "Hash verification failed")
  | `Error s ->
      k (`Error s)
  end

let record_block store peers pieces i off s k =
  let j = off / block_size in
  match pieces.(i).state with
  | Pending ->
      Log.warn "Received block #%d for piece #%d, not requested ???" j i;
      k `Ok
  | Verified ->
      Log.warn "Received block #%d for piece #%d, already completed" j i;
      k `Ok
  | Active parts ->
      let c = parts.(j) in
      if c >= 0 then begin
        parts.(j) <- (-1);
        let rec cancel _ p =
          if Peer.requested p i j (Cstruct.len s) then
            Peer.cancel p i j (Cstruct.len s)
        in
        if c > 1 then Hashtbl.iter cancel peers;
        pieces.(i).have <- pieces.(i).have + 1;
        Log.info "Received block #%d for piece #%d, have %d, missing %d"
          j i pieces.(i).have (Array.length parts - pieces.(i).have)
      end;
      let off = Int64.(add pieces.(i).offset (of_int off)) in
      Store.write store off s (function
      | `Ok ->
          if pieces.(i).have = Array.length parts then
            verify_piece store peers pieces i (function
              | `Ok () -> k `Piece_complete
              | `Error s -> k (`Error s)
            )
          else
            ()
      | `Error s ->
          k (`Error s)
      )

let request_rejected pieces (i, off, _) =
  match pieces.(i).state with
  | Verified
  | Pending -> ()
  | Active parts ->
      let j = off / block_size in
      if parts.(j) > 0 then parts.(j) <- parts.(j) - 1

let make_piece m i =
  { state = Pending;
    length = Metadata.piece_length m i;
    offset = Metadata.offset m i 0;
    have = 0;
    hash = Metadata.hash m i }
