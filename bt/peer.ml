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

module Cs = Nocrypto.Uncommon.Cs

module Speedometer : sig

  type t

  val create : ?resolution:int -> ?seconds:int -> unit -> t
  val add : t -> int -> unit
  val speed : t -> float

end = struct

  let max_tick = 65535 (* 0xFFFF *)

  type t =
    { resolution : float;
      size : int;
      mutable last : float;
      mutable pointer : int;
      buffer : int array }

  let create ?(resolution = 4) ?(seconds = 5) () =
    if resolution <= 0 || seconds <= 0 then invalid_arg "Speedometer.create";
    let size = seconds * resolution in
    let resolution = float resolution in
    let last = (Unix.gettimeofday () -. 1.) *. resolution in
    { resolution; size; last; buffer = Array.make size 0; pointer = 0 }

  let update t =
    let now = Unix.gettimeofday () *. t.resolution in
    let dist = int_of_float (now -. t.last) land max_tick in
    let dist = if dist > t.size then t.size else dist in
    t.last <- now;

    let rec copy dist pointer =
      if dist > 0 then begin
        let pointer = if pointer = t.size - 1 then 0 else pointer + 1 in
        t.buffer.(pointer) <- t.buffer.(if pointer = 0 then t.size - 1 else pointer - 1);
        copy (dist - 1) pointer
      end else
        pointer
    in
    t.pointer <- copy dist t.pointer

  let add t delta =
    update t;
    t.buffer.(t.pointer) <- t.buffer.(t.pointer) + delta

  let speed t =
    update t;
    let top = t.buffer.(t.pointer) in
    let btm = t.buffer.(if t.pointer = t.size - 1 then 0 else t.pointer + 1) in
    float (top - btm) *. t.resolution /. float t.size
    (* = float (top - btm) /. t.seconds *)

end

module Wire : sig
  type message =
    | KEEP_ALIVE
    | CHOKE
    | UNCHOKE
    | INTERESTED
    | NOT_INTERESTED
    | HAVE of int
    | BITFIELD of Bits.t
    | REQUEST of int * int * int
    | PIECE of int * int * Cstruct.t
    | CANCEL of int * int * int
    | PORT of int
    | HAVE_ALL
    | HAVE_NONE
    | SUGGEST of int
    | REJECT of int * int * int
    | ALLOWED of int list
    | EXTENDED of int * Cstruct.t

  val string_of_message : message -> string
  val writer : message -> Util.W.t

  val ltep_bit : int
  val dht_bit : int

  module R : sig
    type state
    val empty : state
    val handle : state -> Cstruct.t -> state * message list
  end
end = struct
  type message =
    | KEEP_ALIVE
    | CHOKE
    | UNCHOKE
    | INTERESTED
    | NOT_INTERESTED
    | HAVE of int
    | BITFIELD of Bits.t
    | REQUEST of int * int * int
    | PIECE of int * int * Cstruct.t
    | CANCEL of int * int * int
    | PORT of int
    | HAVE_ALL
    | HAVE_NONE
    | SUGGEST of int
    | REJECT of int * int * int
    | ALLOWED of int list
    | EXTENDED of int * Cstruct.t

  let strl f l =
    "[" ^ String.concat " " (List.map f l) ^ "]"

  let string_of_message x =
    let open Printf in
    match x with
    | KEEP_ALIVE -> "keep alive"
    | CHOKE -> "choke"
    | UNCHOKE -> "unchoke"
    | INTERESTED -> "interested"
    | NOT_INTERESTED -> "not interested"
    | HAVE i -> sprintf "have %d" i
    | BITFIELD b -> sprintf "bitfield with %d/%d pieces" (Bits.count b) (Bits.length b)
    | REQUEST (i, off, len) -> sprintf "request %d off:%d len:%d" i off len
    | PIECE (i, off, _) -> sprintf "piece %d off:%d" i off
    | CANCEL (i, off, len) -> sprintf "cancel %d off:%d len:%d" i off len
    | PORT port -> sprintf "port %d" port
    | HAVE_ALL -> "have all"
    | HAVE_NONE -> "have none"
    | SUGGEST i -> sprintf "suggest %d" i
    | REJECT (i, off, len) -> sprintf "reject %d off:%d len:%d" i off len
    | ALLOWED pieces -> sprintf "allowed %s" (strl string_of_int pieces)
    | EXTENDED (id, _) -> sprintf "extended %d" id

  let writer x =
    let open Util.W in
    match x with
    | KEEP_ALIVE ->
        empty
    | CHOKE ->
        byte 0
    | UNCHOKE ->
        byte 1
    | INTERESTED ->
        byte 2
    | NOT_INTERESTED ->
        byte 3
    | HAVE i ->
        byte 4 <+> int i
    | BITFIELD bits ->
        byte 5 <+> string (Bits.to_bin bits)
    | REQUEST (i, off, len) ->
        byte 6 <+> int i <+> int off <+> int len
    | PIECE (i, off, s) ->
        byte 7 <+> int i <+> int off <+> immediate s
    | CANCEL (i, off, len) ->
        byte 8 <+> int i <+> int off <+> int len
    | PORT i ->
        byte 9 <+> int16 i
    | SUGGEST i ->
        byte 13 <+> int i
    | HAVE_ALL ->
        byte 14
    | HAVE_NONE ->
        byte 15
    | REJECT (i, off, len) ->
        byte 16 <+> int i <+> int off <+> int len
    | ALLOWED pieces ->
        byte 17 <+> concat (List.map int pieces)
    | EXTENDED (id, s) ->
        byte 20 <+> byte id <+> immediate s

  let writer x =
    let open Util.W in
    let w = writer x in
    int (len w) <+> w

  let parse cs =
    let int cs o = Int32.to_int @@ Cstruct.BE.get_uint32 cs o in
    match Cstruct.get_uint8 cs 0 with
    | 0 ->
        CHOKE
    | 1 ->
        UNCHOKE
    | 2 ->
        INTERESTED
    | 3 ->
        NOT_INTERESTED
    | 4 ->
        HAVE (int cs 1)
    | 5 ->
        BITFIELD (Bits.of_bin @@ Cstruct.to_string @@ Cstruct.shift cs 1)
    | 6 ->
        REQUEST (int cs 1, int cs 5, int cs 9)
    | 7 ->
        PIECE (int cs 1, int cs 5, Cstruct.shift cs 9)
    | 8 ->
        CANCEL (int cs 1, int cs 5, int cs 9)
    | 9 ->
        PORT (Cstruct.BE.get_uint16 cs 1)
    | 13 ->
        SUGGEST (int cs 1)
    | 14 ->
        HAVE_ALL
    | 15 ->
        HAVE_NONE
    | 16 ->
        REJECT (int cs 1, int cs 5, int cs 9)
    | 17->
        let rec loop cs =
          if Cstruct.len cs >= 4 then
            let p, cs = Cstruct.split cs 4 in
            int p 0 :: loop cs
          else
            []
        in
        ALLOWED (loop @@ Cstruct.shift cs 1)
    | 20 ->
        EXTENDED (Cstruct.get_uint8 cs 1, Cstruct.shift cs 2)
    | _ ->
        failwith "can't parse msg"

  let parse cs =
    if Cstruct.len cs = 0 then
      KEEP_ALIVE
    else
      parse cs

  let max_packet_len = 32 * 1024

  let ltep_bit = 43 (* 20-th bit from the right *)
  let dht_bit = 63 (* last bit of the extension bitfield *)

  module R = struct

    type state = Cstruct.t

    let empty = Cs.empty

    let (<+>) = Cs.(<+>)

    let handle state buf =
      let rec loop cs =
        if Cstruct.len cs > 4 then
          let l = Int32.to_int @@ Cstruct.BE.get_uint32 cs 0 in
          if l + 4 >= Cstruct.len cs then
            let packet, cs = Cstruct.split (Cstruct.shift cs 4) l in
            let cs, packets = loop cs in
            let packet = parse packet in
            cs, packet :: packets
          else
            cs, []
        else
          cs, []
      in
      loop (state <+> buf)

  end
end

let keepalive_delay = 20. (* FIXME *)
let request_pipeline_max = 5
let info_piece_size = 16 * 1024
let default_block_size = 16 * 1024

open Event

type addr = Unix.inet_addr * int

type has_meta_info =
  { have : Bits.t;
    blame : Bits.t;
    meta : Metadata.t }

and no_meta_info =
  { mutable have : int list;
    mutable has_all : bool }

and meta_info =
  | HasMeta of has_meta_info
  | NoMeta of no_meta_info

and t =
  { id : SHA1.t;

    mutable am_choking : bool;
    mutable am_interested : bool;
    mutable peer_choking : bool;
    mutable peer_interested : bool;

    extbits : Bits.t;
    extensions : (string, int) Hashtbl.t;

    mutable strikes : int;

    mutable info : meta_info;

    mutable upload : Speedometer.t;
    mutable download : Speedometer.t;

    mutable time : float;
    mutable piece_data_time : float;

    mutable last_pex : addr list;

    send : unit Lwt_condition.t;
    queue : Wire.message Lwt_sequence.t }

let string_of_node (id, (ip, port)) =
  Printf.sprintf "%s (%s:%d)" (SHA1.to_hex_short id) (Unix.string_of_inet_addr ip) port

let strl f l =
  "[" ^ String.concat " " (List.map f l) ^ "]"

let ut_pex = "ut_pex"
let ut_metadata = "ut_metadata"

let supports p name =
  Hashtbl.mem p.extensions name

let id p =
  p.id

let peer_choking p =
  p.peer_choking

let peer_interested p =
  p.peer_interested

let has_piece p idx =
  assert (0 <= idx);
  match p.info with
  | HasMeta nfo -> Bits.is_set nfo.have idx
  | NoMeta nfo -> nfo.has_all || List.mem idx nfo.have

let has p =
  match p.info with
  | HasMeta nfo -> Bits.copy nfo.have
  | NoMeta _ -> failwith "Peer.have: no meta info"

let am_choking p =
  p.am_choking

let am_interested p =
  p.am_interested


let is_snubbing p =
  let now = Unix.time () in
  now -. p.piece_data_time <= 30.0

let to_string p =
  SHA1.to_hex_short p.id
  (* string_of_node p.node *)

let download_speed p =
  Speedometer.speed p.download

let upload_speed p =
  Speedometer.speed p.upload

let is_seeder p =
  false

let create id info =
  { id;
    am_choking = true; am_interested = false;
    peer_choking = true; peer_interested = false;
    extbits = Bits.create (8 * 8);
    extensions = Hashtbl.create 3;
    info;
    download = Speedometer.create ();
    upload = Speedometer.create ();
    strikes = 0;
    time = Unix.time ();
    piece_data_time = 0.0;
    last_pex = [];
    send = Lwt_condition.create ();
    queue = Lwt_sequence.create () }

let create id =
  let info = NoMeta { have = []; has_all = false } in
  create id info

(* let create_has_meta id m = *)
(*   let npieces = Metadata.piece_count m in *)
(*   let info = HasMeta *)
(*       { have = Bits.create npieces; *)
(*         blame = Bits.create npieces; *)
(*         meta = m } *)
(*   in *)
(*   create id info *)

let worked_on_piece p i =
  match p.info with
  | HasMeta info ->
      Bits.is_set info.blame i
  | NoMeta _ ->
      false

let strike p =
  p.strikes <- p.strikes + 1;
  p.strikes

let seeding p =
  match p.info with
  | NoMeta _ -> false
  | HasMeta info -> Bits.has_all info.have

let time p =
  p.time

let piece_data_time p =
  p.piece_data_time

(* Incoming *)

let got_ut_metadata p data =
  let m, data = Bcode.decode_partial data in
  let msg_type = Bcode.to_int (Bcode.find "msg_type" m) in
  let piece = Bcode.to_int (Bcode.find "piece" m) in
  match msg_type with
  | 0 -> (* request *)
      MetaRequested (p.id, piece)
  | 1 -> (* data *)
      GotMetaPiece (p.id, piece, data)
  | 2 -> (* reject *)
      RejectMetaPiece (p.id, piece)
  | _ ->
      NoEvent

let got_ut_pex p data =
  (* FIXME support for IPv6 *)
  let m = Bcode.decode data in
  let added = Bcode.find "added" m |> Bcode.to_cstruct in
  let added_f = Bcode.find "added.f" m |> Bcode.to_cstruct in
  let dropped = Bcode.find "dropped" m |> Bcode.to_cstruct in
  let rec loop cs =
    if Cstruct.len cs >= 6 then
      let addr, cs = Cstruct.split cs 6 in
      let ip =
        Unix.inet_addr_of_string
          (Printf.sprintf "%d.%d.%d.%d"
            (Cstruct.get_uint8 addr 0) (Cstruct.get_uint8 cs 1)
            (Cstruct.get_uint8 addr 2) (Cstruct.get_uint8 cs 3))
      in
      let port = Cstruct.BE.get_uint16 cs 4 in
      (ip, port) :: loop cs
    else
      []
  in
  let flag n =
    { pex_encryption = n land 0x1 <> 0;
      pex_seed = n land 0x2 <> 0;
      pex_utp = n land 0x4 <> 0;
      pex_holepunch = n land 0x8 <> 0;
      pex_outgoing = n land 0x10 <> 0 }
  in
  let added = loop added in
  let added_f =
    let rec loop i =
      if i >= Cstruct.len added_f then
        []
      else
        flag (Cstruct.get_uint8 added_f i) :: loop (i + 1)
    in
    loop 0
  in
  GotPEX (p.id, List.combine added added_f, loop dropped)

let supported_extensions =
  [ 1, ("ut_metadata", got_ut_metadata);
    2, ("ut_pex", got_ut_pex) ]

let got_choke p =
  if not p.peer_choking then begin
    p.peer_choking <- true;
    Choked p.id
  end else
    NoEvent

let got_unchoke p =
  if p.peer_choking then begin
    (* debug "%s is no longer choking us" (string_of_node p.node); *)
    p.peer_choking <- false;
    Unchoked p.id
  end else
    NoEvent

let got_interested p =
  if not p.peer_interested then begin
    (* debug "%s is interested in us" (string_of_node p.node); *)
    p.peer_interested <- true;
    Interested p.id
  end else
    NoEvent

let got_not_interested p =
  if p.peer_interested then begin
    (* debug "%s is no longer interested in us" (string_of_node p.node); *)
    p.peer_interested <- false;
    NotInterested p.id
  end else
    NoEvent

let got_have_bitfield p b =
  begin match p.info with
  | HasMeta n ->
      Bits.blit b 0 n.have 0 (Bits.length n.have);
      HaveBitfield (p.id, n.have)
  | NoMeta n ->
      let rec loop acc i =
        if i >= Bits.length b then List.rev acc else
        if Bits.is_set b i then loop (i :: acc) (i+1)
        else loop acc (i+1)
      in
      n.have <- loop [] 0;
      HaveBitfield (p.id, b)
  end

let got_have p idx =
  match p.info with
  | HasMeta nfo ->
      if not (Bits.is_set nfo.have idx) then begin
        Bits.set nfo.have idx;
        Have (p.id, idx)
      end else
        NoEvent
  | NoMeta n ->
      if not (List.mem idx n.have) && not n.has_all then begin
        n.have <- idx :: n.have;
        Have (p.id, idx)
      end else
        NoEvent

let got_cancel p i ofs len =
  (* Lwt_sequence.iter_node_l (fun n -> *)
  (*   match Lwt_sequence.get n with *)
  (*   | Wire.PIECE (i1, ofs1, s) when i = i1 && ofs = ofs1 && Cstruct.len s = len -> *)
  (*       Lwt_sequence.remove n *)
  (*   | _ -> *)
  (*       ()) p.send_queue; *)
  (* FIXME broadcast event *)
  NoEvent

let got_piece p idx off s =
  (* p.act_reqs <- p.act_reqs - 1; *)
  p.piece_data_time <- Unix.time ();
  Speedometer.add p.download (Cstruct.len s);
  match p.info with
  | HasMeta info ->
      Bits.set info.blame idx;
      BlockReceived (p.id, idx, Metadata.block_number info.meta idx off, Cstruct.to_string s)
  | NoMeta _ ->
      failwith "Peer.got_piece: no meta info"

let got_extended_handshake p bc =
  let m =
    Bcode.find "m" bc |> Bcode.to_dict |>
    List.map (fun (name, id) -> (name, Bcode.to_int id))
  in
  List.iter (fun (name, id) ->
    if id = 0 then Hashtbl.remove p.extensions name
    else Hashtbl.replace p.extensions name id) m;
  (* debug "%s supports %s" (string_of_node p.node) (strl fst m); *)
  if Hashtbl.mem p.extensions "ut_metadata" then
    AvailableMetadata (p.id, Bcode.find "metadata_size" bc |> Bcode.to_int)
  else
    NoEvent

let got_extended p id data =
  let (_, f) = List.assoc id supported_extensions in
  f p data

let got_request p idx off len =
  match p.info with
  | HasMeta info ->
      let b = Metadata.block_number info.meta idx off in
      let _, _, l = Metadata.block info.meta idx b in
      assert (l = len);
      BlockRequested (p.id, idx, b)
  | _ ->
      NoEvent
(* FIXME send REJECT if fast extension is supported *)

let got_port p i =
  DHTPort (p.id, i)

let got_message p m =
  match m with
  | Wire.KEEP_ALIVE              -> NoEvent
  | Wire.CHOKE                   -> got_choke p
  | Wire.UNCHOKE                 -> got_unchoke p
  | Wire.INTERESTED              -> got_interested p
  | Wire.NOT_INTERESTED          -> got_not_interested p
  | Wire.HAVE i                  -> got_have p i
  | Wire.BITFIELD b              -> got_have_bitfield p b
  | Wire.REQUEST (idx, off, len) -> got_request p idx off len
  | Wire.PIECE (idx, off, s)     -> got_piece p idx off s
  | Wire.CANCEL (idx, off, len)  -> got_cancel p idx off len
  (* | Wire.HAVE_ALL *)
  (* | Wire.HAVE_NONE -> raise (InvalidProtocol m) *)
  | Wire.EXTENDED (0, s)         -> got_extended_handshake p (Bcode.decode s)
  | Wire.EXTENDED (id, s)        -> got_extended p id s
  | Wire.PORT i                  -> got_port p i
  | _                            -> NoEvent

(* Outgoing *)

let send p m =
  ignore (Lwt_sequence.add_r m p.queue);
  Lwt_condition.signal p.send ()

let send_block p i b s =
  match p.info with
  | HasMeta info ->
      let i, o, l = Metadata.block info.meta i b in
      assert (l = String.length s);
      send p @@ Wire.PIECE (i, o, Cstruct.of_string s)
  | _ ->
      assert false

let send_request p (i, ofs, len) =
  (* p.act_reqs <- p.act_reqs + 1; *)
  Wire.REQUEST (i, ofs, len)

let extended_handshake p =
  let m =
    List.map (fun (id, (name, _)) ->
      name, Bcode.Int (Int64.of_int id)) supported_extensions
  in
  let m = Bcode.Dict ["m", Bcode.Dict m] in
  send p @@ Wire.EXTENDED (0, Bcode.encode m)

let choke p =
  match p.am_choking with
  | false ->
      p.am_choking <- true;
      (* debug "choking %s" (string_of_node p.node); *)
      send p Wire.CHOKE
  | true ->
      ()

let unchoke p =
  match p.am_choking with
  | true ->
      p.am_choking <- false;
      (* debug "no longer choking %s" (string_of_node p.node); *)
      send p Wire.UNCHOKE
  | false ->
      ()

let interested p =
  match p.am_interested with
  | false ->
      p.am_interested <- true;
      (* debug "interested in %s" (string_of_node p.node); *)
      send p Wire.INTERESTED
  | true ->
      ()

let not_interested p =
  match p.am_interested with
  | true ->
      p.am_interested <- false;
      (* debug "no longer interested in %s" (string_of_node p.node); *)
      send p Wire.NOT_INTERESTED
  | false ->
      ()

let have p idx =
  match has_piece p idx with
  | false ->
      send p @@ Wire.HAVE idx
  | true ->
      ()

let have_bitfield p bits =
  send p @@ Wire.BITFIELD bits

let send_cancel p (i, j) =
  match p.info with
  | HasMeta n ->
      let i, ofs, len = Metadata.block n.meta i j in
      send p @@ Wire.CANCEL (i, ofs, len)
  | NoMeta _ ->
      failwith "send_cancel: no meta info"

let send_port p i =
  send p @@ Wire.PORT i

let request_metadata_piece p idx =
  assert (idx >= 0);
  assert (Hashtbl.mem p.extensions "ut_metadata");
  let id = Hashtbl.find p.extensions "ut_metadata" in
  let d =
    [ "msg_type", Bcode.Int 0L;
      "piece", Bcode.Int (Int64.of_int idx) ]
  in
  send p @@ Wire.EXTENDED (id, Bcode.encode @@ Bcode.Dict d)

let send_ut_pex p added dropped =
  let id = Hashtbl.find p.extensions "ut_pex" in
  let rec c (ip, port) =
    let cs =
      Scanf.sscanf (Unix.string_of_inet_addr ip) "%d.%d.%d.%d"
        (fun a b c d ->
           let cs = Cstruct.create 6 in
           Cstruct.set_uint8 cs 0 a; Cstruct.set_uint8 cs 1 b;
           Cstruct.set_uint8 cs 2 a; Cstruct.set_uint8 cs 3 d;
           cs)
    in
    Cstruct.BE.set_uint16 cs 4 port;
    cs
  in
  let c l = Cs.concat (List.map c l) in
  let d =
    [ "added", Bcode.String (c added);
      "added.f", Bcode.String (Cs.create_with (List.length added) 0);
      "dropped", Bcode.String (c dropped) ]
  in
  send p @@ Wire.EXTENDED (id, Bcode.encode @@ Bcode.Dict d)
  (* debug "sent pex to %s added %d dropped %d" (string_of_node p.node) *)
    (* (List.length added) (List.length dropped) *)

let send_pex pex p =
  if supports p ut_pex then begin
    let added = List.filter (fun a -> not (List.mem a p.last_pex)) pex in
    let dropped = List.filter (fun a -> not (List.mem a pex)) p.last_pex in
    p.last_pex <- pex;
    send_ut_pex p added dropped
  end

let reject_metadata_request p piece =
  let id = Hashtbl.find p.extensions "ut_metadata" in
  let m =
    let d = [ "msg_type", Bcode.Int 2L; "piece", Bcode.Int (Int64.of_int piece) ] in
    Bcode.encode (Bcode.Dict d)
  in
  send p @@ Wire.EXTENDED (id, m)

let metadata_piece len i data p =
  let id = Hashtbl.find p.extensions "ut_metadata" in
  let m =
    let d =
      [ "msg_type",   Bcode.Int 1L;
        "piece",      Bcode.Int (Int64.of_int i);
        "total_size", Bcode.Int (Int64.of_int len) ]
    in
    Cs.(Bcode.encode (Bcode.Dict d) <+> data)
  in
  send p @@ Wire.EXTENDED (id, m)

(* Event loop *)

let (>>=) = Lwt.(>>=)

let buf_size = 1024

let handle_err p push fd e =
  Printf.eprintf "unexpected exc: %S\n%!" (Printexc.to_string e);
  push (PeerDisconnected p.id);
  Lwt_unix.close fd

let reader_loop p push fd =
  let buf = Cstruct.create buf_size in
  let rec loop r =
    Lwt_unix.with_timeout keepalive_delay
      (fun () -> Lwt_cstruct.read fd buf) >>= function
    | 0 ->
        failwith "eof"
    | n ->
        let r, msgs = Wire.R.handle r (Cstruct.sub buf 0 n) in
        List.iter (fun msg -> push (got_message p msg)) msgs;
        loop r
  in
  loop Wire.R.empty

let reader_loop p push fd =
  Lwt.catch (fun () -> reader_loop p push fd) (handle_err p push fd)

let writer_loop p fd =
  let write m = Lwt_cstruct.(complete (write fd) @@ Util.W.to_cstruct (Wire.writer m)) in
  let rec loop () =
    Lwt.pick
      [ (Lwt_condition.wait p.send >>= fun () -> Lwt.return `Ok);
        (Lwt_unix.sleep keepalive_delay >>= fun () -> Lwt.return `Timeout) ]
    >>= function
    | `Ok ->
        Lwt_sequence.fold_l (fun m t -> t >>= fun () -> write m) p.queue Lwt.return_unit >>=
        loop
    | `Timeout ->
        write Wire.KEEP_ALIVE >>=
        loop
  in
  loop ()

let writer_loop p push fd =
  Lwt.catch (fun () -> writer_loop p fd) (handle_err p push fd)

let start p push fd =
  ignore (reader_loop p push fd);
  ignore (writer_loop p push fd)

let stop p =
  assert false
