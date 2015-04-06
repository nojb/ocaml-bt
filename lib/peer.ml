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

module Log  = Log.Make (struct let section = "[Peer]" end)
module Cs   = Nocrypto.Uncommon.Cs
module ARC4 = Nocrypto.Cipher_stream.ARC4

type addr = Unix.inet_addr * int

type pex_flags =
  { pex_encryption : bool;
    pex_seed : bool;
    pex_utp : bool;
    pex_holepunch : bool;
    pex_outgoing : bool }

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

  (* val ltep_bit : int *)
  (* val dht_bit : int *)

  val max_packet_len : int

  val handle : Cstruct.t -> Cstruct.t * message list

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
    | BITFIELD b -> sprintf "bitfield with %d/%d pieces" (Bits.count_ones b) (Bits.length b)
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
    | KEEP_ALIVE            -> empty
    | CHOKE                 -> byte 0
    | UNCHOKE               -> byte 1
    | INTERESTED            -> byte 2
    | NOT_INTERESTED        -> byte 3
    | HAVE i                -> byte 4 <+> int i
    | BITFIELD bits         -> byte 5 <+> immediate (Bits.to_cstruct bits)
    | REQUEST (i, off, len) -> byte 6 <+> int i <+> int off <+> int len
    | PIECE (i, off, s)     -> byte 7 <+> int i <+> int off <+> immediate s
    | CANCEL (i, off, len)  -> byte 8 <+> int i <+> int off <+> int len
    | PORT i                -> byte 9 <+> int16 i
    | SUGGEST i             -> byte 13 <+> int i
    | HAVE_ALL              -> byte 14
    | HAVE_NONE             -> byte 15
    | REJECT (i, off, len)  -> byte 16 <+> int i <+> int off <+> int len
    | ALLOWED pieces        -> byte 17 <+> concat (List.map int pieces)
    | EXTENDED (id, s)      -> byte 20 <+> byte id <+> immediate s

  let writer x =
    let open Util.W in
    let w = writer x in
    int (len w) <+> w

  let parse_allowed cs =
    let rec loop o =
      if Cstruct.len cs >= o + 4 then
        let p = Int32.to_int @@ Cstruct.BE.get_uint32 cs o in
        p :: loop (o + 4)
      else
        []
    in
    loop 0

  let parse cs =
    let int cs o = Int32.to_int @@ Cstruct.BE.get_uint32 cs o in
    match Cstruct.get_uint8 cs 0 with
    | 00 -> CHOKE
    | 01 -> UNCHOKE
    | 02 -> INTERESTED
    | 03 -> NOT_INTERESTED
    | 04 -> HAVE (int cs 1)
    | 05 -> BITFIELD (Bits.of_cstruct @@ Cstruct.shift cs 1)
    | 06 -> REQUEST (int cs 1, int cs 5, int cs 9)
    | 07 -> PIECE (int cs 1, int cs 5, Cstruct.shift cs 9)
    | 08 -> CANCEL (int cs 1, int cs 5, int cs 9)
    | 09 -> PORT (Cstruct.BE.get_uint16 cs 1)
    | 13 -> SUGGEST (int cs 1)
    | 14 -> HAVE_ALL
    | 15 -> HAVE_NONE
    | 16 -> REJECT (int cs 1, int cs 5, int cs 9)
    | 17 -> ALLOWED (parse_allowed @@ Cstruct.shift cs 1)
    | 20 -> EXTENDED (Cstruct.get_uint8 cs 1, Cstruct.shift cs 2)
    | _  -> failwith "can't parse msg"

  let parse cs =
    if Cstruct.len cs = 0 then
      KEEP_ALIVE
    else
      parse cs

  let max_packet_len = 1 lsl 15 (* 32 * 1024 = 32768 *)

  (* let ltep_bit = 43 (\* 20-th bit from the right *\) *)
  (* let dht_bit = 63 (\* last bit of the extension bitfield *\) *)

  let rec handle cs =
    if Cstruct.len cs >= 4 then
      let l = Int32.to_int @@ Cstruct.BE.get_uint32 cs 0 in
      if l + 4 <= Cstruct.len cs then
        let packet, cs = Cstruct.split (Cstruct.shift cs 4) l in
        let cs, packets = handle cs in
        let packet = parse packet in
        cs, packet :: packets
      else
        cs, []
    else
      cs, []

end

let keepalive_delay = 20. (* FIXME *)
let request_pipeline_max = 5
let info_piece_size = 16 * 1024
let default_block_size = 16 * 1024

type event =
  | Choked of (int * int * int) list
  | Unchoked
  | Interested
  | NotInterested
  | Have of int
  | HaveBitfield of Bits.t
  | BlockRequested of int * int * int
  | BlockReceived of int * int * Cstruct.t
  | PeerDisconnected of (int * int * int) list
  | AvailableMetadata of int
  | MetaRequested of int
  | GotMetaPiece of int * Cstruct.t
  | RejectMetaPiece of int
  | GotPEX of (addr * pex_flags) list * addr list
  | DHTPort of int

type t =
  { id : SHA1.t;

    mutable am_choking : bool;
    mutable am_interested : bool;
    mutable peer_choking : bool;
    mutable peer_interested : bool;

    blame : Bits.t;
    have : Bits.t;
    extbits : Bits.t;
    extensions : (string, int) Hashtbl.t;

    mutable strikes : int;

    mutable uploaded : int64;
    mutable downloaded : int64;
    upload : Speedometer.t;
    download : Speedometer.t;

    mutable last_pex : addr list;

    requests : (int * int * int) Lwt_sequence.t;
    peer_requests : (int * int * int) Lwt_sequence.t;

    send : unit Lwt_condition.t;
    queue : Wire.message Lwt_sequence.t;

    push : event -> unit }

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

let has p i =
  if i < 0 then invalid_arg "Peer.has";
  Bits.is_set p.have i

let bitfield p =
  p.have

let am_choking p =
  p.am_choking

let am_interested p =
  p.am_interested

let to_string p =
  SHA1.to_hex_short p.id
  (* string_of_node p.node *)

let download_speed p =
  Speedometer.speed p.download

let upload_speed p =
  Speedometer.speed p.upload

let requests p =
  Lwt_sequence.length p.requests

let requested p i off len =
  match Lwt_sequence.find_node_opt_l (fun (i', off', len') -> i = i' && off = off' && len = len') p.requests with
  | None ->
      false
  | Some _ ->
      true

let worked_on_piece p i =
  Bits.is_set p.blame i

let strike p =
  p.strikes <- p.strikes + 1;
  p.strikes

let got_ut_metadata p data =
  let m, data = Bcode.decode_partial data in
  let msg_type = Bcode.to_int (Bcode.find "msg_type" m) in
  let piece = Bcode.to_int (Bcode.find "piece" m) in
  match msg_type with
  | 0 -> (* request *)
      p.push @@ MetaRequested piece
  | 1 -> (* data *)
      p.push @@ GotMetaPiece (piece, data)
  | 2 -> (* reject *)
      p.push @@ RejectMetaPiece piece
  | _ ->
      ()

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
  p.push @@ GotPEX (List.combine added added_f, loop dropped)

let supported_extensions =
  [ 1, ("ut_metadata", got_ut_metadata);
    2, ("ut_pex", got_ut_pex) ]

(* Outgoing *)

let send p m =
  ignore (Lwt_sequence.add_r m p.queue);
  Lwt_condition.signal p.send ()

let piece p i o buf =
  p.uploaded <- Int64.add p.uploaded (Int64.of_int @@ Cstruct.len buf);
  Speedometer.add p.upload (Cstruct.len buf);
  Log.info "> PIECE id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i o (Cstruct.len buf);
  (* TODO emit Uploaded event *)
  send p @@ Wire.PIECE (i, o, buf)

let request p i ofs len =
  if p.peer_choking then invalid_arg "Peer.request";
  Log.info "> REQUESTING id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i ofs len;
  ignore (Lwt_sequence.add_r (i, ofs, len) p.requests);
  send p @@ Wire.REQUEST (i, ofs, len)

let extended_handshake p =
  let m =
    List.map (fun (id, (name, _)) ->
      name, Bcode.Int (Int64.of_int id)) supported_extensions
  in
  let m = Bcode.Dict ["m", Bcode.Dict m] in
  Log.info "> EXTENDED HANDSHAKE id:%s (%s)" (SHA1.to_hex_short p.id)
    (String.concat " " (List.map (fun (n, (name, _)) -> Printf.sprintf "%s %d" name n) supported_extensions));
  send p @@ Wire.EXTENDED (0, Bcode.encode m)

let choke p =
  if not p.am_choking then begin
    p.am_choking <- true;
    Lwt_sequence.iter_node_l Lwt_sequence.remove p.peer_requests;
    Log.info "> CHOKING id:%s" (SHA1.to_hex_short p.id);
    send p Wire.CHOKE
  end

let unchoke p =
  if p.am_choking then begin
      p.am_choking <- false;
      Log.info "> UNCHOKING id:%s" (SHA1.to_hex_short p.id);
      send p Wire.UNCHOKE
  end

let interested p =
  if not p.am_interested then begin
      p.am_interested <- true;
      Log.info "> INTERESTED id:%s" (SHA1.to_hex_short p.id);
      send p Wire.INTERESTED
  end

let not_interested p =
  if p.am_interested then begin
      p.am_interested <- false;
      Log.info "> NOT INTERESTED id:%s" (SHA1.to_hex_short p.id);
      send p Wire.NOT_INTERESTED
  end

let have p i =
  Log.info "> HAVE id:%s idx:%d" (SHA1.to_hex_short p.id) i;
  send p @@ Wire.HAVE i

let have_bitfield p bits =
  Bits.set_length p.have (Bits.length bits);
  Bits.set_length p.blame (Bits.length bits);
  Log.info "> BITFIELD id:%s have:%d total:%d" (SHA1.to_hex_short p.id)
    (Bits.count_ones bits) (Bits.length bits);
  send p @@ Wire.BITFIELD bits

let cancel p i o l =
  Log.info "> CANCEL id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i o l;
  send p @@ Wire.CANCEL (i, o, l);
  try
    let n = Lwt_sequence.find_node_l (fun (i', o', l') -> i = i' && o' = o && l = l') p.requests in
    Lwt_sequence.remove n
  with
  | Not_found ->
      Log.warn "! REQUEST NOT FOUND id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i o l

let send_port p i =
  Log.info "PORT id:%s port:%d" (SHA1.to_hex_short p.id) i;
  send p @@ Wire.PORT i

let request_metadata_piece p i =
  if i < 0 || not (Hashtbl.mem p.extensions ut_metadata) then invalid_arg "request_metadata_piece";
  let id = Hashtbl.find p.extensions ut_metadata in
  let d =
    [ "msg_type", Bcode.Int 0L;
      "piece", Bcode.Int (Int64.of_int i) ]
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
  Log.info "> PEX id:%s added:%d dropped:%d" (SHA1.to_hex_short p.id)
    (List.length added) (List.length dropped);
  send p @@ Wire.EXTENDED (id, Bcode.encode @@ Bcode.Dict d)

let send_pex pex p =
  if supports p ut_pex then begin
    let added = List.filter (fun a -> not (List.mem a p.last_pex)) pex in
    let dropped = List.filter (fun a -> not (List.mem a pex)) p.last_pex in
    p.last_pex <- pex;
    send_ut_pex p added dropped
  end

let reject_metadata_request p piece =
  let id = Hashtbl.find p.extensions ut_metadata in
  let m =
    let d = [ "msg_type", Bcode.Int 2L; "piece", Bcode.Int (Int64.of_int piece) ] in
    Bcode.encode (Bcode.Dict d)
  in
  send p @@ Wire.EXTENDED (id, m)

let metadata_piece len i data p =
  let id = Hashtbl.find p.extensions ut_metadata in
  let m =
    let d =
      [ "msg_type",   Bcode.Int 1L;
        "piece",      Bcode.Int (Int64.of_int i);
        "total_size", Bcode.Int (Int64.of_int len) ]
    in
    Cs.(Bcode.encode (Bcode.Dict d) <+> data)
  in
  send p @@ Wire.EXTENDED (id, m)

(* Incoming *)

let on_choke p =
  if not p.peer_choking then begin
    p.peer_choking <- true;
    let reqs = Lwt_sequence.fold_l (fun r l -> r :: l) p.requests [] in
    Lwt_sequence.iter_node_l (fun n -> Lwt_sequence.remove n) p.requests;
    p.push @@ Choked reqs
  end

let on_unchoke p =
  if p.peer_choking then begin
    Log.info "< UNCHOKE id:%s" (SHA1.to_hex_short p.id);
    p.peer_choking <- false;
    p.push @@ Unchoked
  end

let on_interested p =
  if not p.peer_interested then begin
    Log.info "< INTERESTED id:%s" (SHA1.to_hex_short p.id);
    p.peer_interested <- true;
    p.push @@ Interested
  end

let on_not_interested p =
  if p.peer_interested then begin
    Log.info "< NOT INTERESTED id:%s" (SHA1.to_hex_short p.id);
    p.peer_interested <- false;
    p.push @@ NotInterested
  end

let on_have p i =
  Bits.resize p.have (i + 1);
  if not (Bits.is_set p.have i) then begin
    Log.info "< HAVE id:%s idx:%d" (SHA1.to_hex_short p.id) i;
    Bits.set p.have i;
    p.push @@ Have i
  end

let on_bitfield p b =
  Bits.set_length p.have (Bits.length b);
  Bits.blit b 0 p.have 0 (Bits.length b);
  Log.info "< BITFIELD id:%s have:%d total:%d" (SHA1.to_hex_short p.id) (Bits.count_ones b) (Bits.length b);
  p.push @@ HaveBitfield p.have

let on_request p i off len =
  if not p.am_choking then begin
    let (_ : _ Lwt_sequence.node) = Lwt_sequence.add_r (i, off, len) p.peer_requests in
    Log.info "< REQUEST id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i off len;
    p.push @@ BlockRequested (i, off, len)
  end

let on_piece p i off s =
  begin match
    Lwt_sequence.find_node_opt_l
      (fun (i', off', len') -> i = i && off = off' && len' = Cstruct.len s)
      p.requests
  with
  | None -> ()
  | Some n -> Lwt_sequence.remove n
  end;
  p.downloaded <- Int64.add p.downloaded (Int64.of_int @@ Cstruct.len s);
  Speedometer.add p.download (Cstruct.len s);
  Bits.resize p.blame (i + 1);
  Bits.set p.blame i;
  (* TODO emit Downloaded event *)
  Log.info "< PIECE id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i off (Cstruct.len s);
  p.push @@ BlockReceived (i, off, s)

let on_cancel p i off len =
  Log.info "< CANCEL id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i off len;
  let n =
    Lwt_sequence.find_node_opt_l
      (fun (i', off', len') -> i = i && off = off' && len = len')
      p.peer_requests
  in
  match n with
  | Some n ->
      (* FIXME broadcast event *)
      Lwt_sequence.remove n
  | None ->
      Log.warn "! PEER REQUEST NOT FOUND id:%s idx:%d off:%d len:%d"
        (SHA1.to_hex_short p.id) i off len

let on_extended_handshake p s =
  let bc = Bcode.decode s in
  let m =
    Bcode.find "m" bc |> Bcode.to_dict |>
    List.map (fun (name, id) -> (name, Bcode.to_int id))
  in
  List.iter (fun (name, id) ->
    if id = 0 then Hashtbl.remove p.extensions name
    else Hashtbl.replace p.extensions name id) m;
  Log.info "< EXTENDED HANDSHAKE id:%s (%s)" (SHA1.to_hex_short p.id)
    (String.concat " " (List.map (fun (name, n) -> Printf.sprintf "%s %d" name n) m));
  if Hashtbl.mem p.extensions ut_metadata then
    p.push @@ AvailableMetadata (Bcode.find "metadata_size" bc |> Bcode.to_int)

let on_extended p id data =
  Log.info "< EXTENDED id:%s mid:%d" (SHA1.to_hex_short p.id) id;
  let (_, f) = List.assoc id supported_extensions in
  f p data

let on_port p i =
  Log.info "< PORT id:%s port:%d" (SHA1.to_hex_short p.id) i;
  p.push @@ DHTPort i

let on_message p m =
  match m with
  | Wire.KEEP_ALIVE            -> ()
  | Wire.CHOKE                 -> on_choke p
  | Wire.UNCHOKE               -> on_unchoke p
  | Wire.INTERESTED            -> on_interested p
  | Wire.NOT_INTERESTED        -> on_not_interested p
  | Wire.HAVE i                -> on_have p i
  | Wire.BITFIELD b            -> on_bitfield p b
  | Wire.REQUEST (i, off, len) -> on_request p i off len
  | Wire.PIECE (i, off, s)     -> on_piece p i off s
  | Wire.CANCEL (i, off, len)  -> on_cancel p i off len
  | Wire.EXTENDED (0, s)       -> on_extended_handshake p s
  | Wire.EXTENDED (id, data)   -> on_extended p id data
  | Wire.PORT i                -> on_port p i
  | _                          -> ()

(* Event loop *)

let encrypt key cs =
  match key with
  | None -> None, cs
  | Some key ->
      let { ARC4.key; message = cs } = ARC4.encrypt ~key cs in
      Some key, cs

let decrypt = encrypt

open Lwt.Infix

let handle_err p fd e =
  Log.error "[%s] exn %s" (SHA1.to_hex_short p.id) (Printexc.to_string e);
  let reqs = Lwt_sequence.fold_l (fun r l -> r :: l) p.requests [] in
  p.push (PeerDisconnected reqs);
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

let reader_loop p fd key =
  let buf = Cstruct.create Wire.max_packet_len in
  let rec loop key data =
    Lwt_unix.with_timeout keepalive_delay (fun () -> Lwt_cstruct.read fd buf) >>= function
    | 0 ->
        Lwt.fail End_of_file
    | n ->
        (* Log.debug "[%s] read %d bytes" (SHA1.to_hex_short p.id) n; *)
        let key, buf = decrypt key (Cstruct.sub buf 0 n) in
        let data, msgs = Wire.handle Cs.(data <+> buf) in
        (* List.iter (fun m -> Log.debug "read message from [%s] : %s" (SHA1.to_hex_short p.id) *)
        (*     (Wire.string_of_message m)) msgs; *)
        List.iter (on_message p) msgs;
        loop key data
  in
  loop key Cs.empty

let writer_loop p fd key =
  let buf = Cstruct.create Wire.max_packet_len in
  let write m key =
    (* Log.debug "sending message to [%s] : %s" (SHA1.to_hex_short p.id) (Wire.string_of_message m); *)
    let buf = Util.W.into_cstruct (Wire.writer m) buf in
    let key, buf = encrypt key buf in
    Lwt_cstruct.(complete (write fd) @@ buf) >>= fun () ->
    Lwt.return key
  in
  let rec loop key =
    Lwt.pick
      [ (Lwt_condition.wait p.send >>= fun () -> Lwt.return `Ok);
        (Lwt_unix.sleep keepalive_delay >>= fun () -> Lwt.return `Timeout) ]
    >>= function
    | `Ok ->
        Lwt_sequence.fold_l (fun m t -> t >>= write m) p.queue (Lwt.return key) >>=
        loop
    | `Timeout ->
        write Wire.KEEP_ALIVE key >>=
        loop
  in
  loop key

let start p fd mode =
  let my_key, her_key =
    match mode with
    | None -> None, None
    | Some (my_key, her_key) -> Some my_key, Some her_key
  in
  Lwt.pick [reader_loop p fd her_key; writer_loop p fd my_key]

let start p fd mode =
  Lwt.ignore_result
    (Lwt.catch (fun () -> start p fd mode) (handle_err p fd))

let create id push fd mode =
  let p =
    { id;
      am_choking = true; am_interested = false;
      peer_choking = true; peer_interested = false;
      extbits = Bits.create (8 * 8);
      extensions = Hashtbl.create 3;
      uploaded = 0L;
      downloaded = 0L;
      download = Speedometer.create ();
      upload = Speedometer.create ();
      strikes = 0;
      blame = Bits.create 0;
      have = Bits.create 0;
      last_pex = [];
      requests = Lwt_sequence.create ();
      peer_requests = Lwt_sequence.create ();
      send = Lwt_condition.create ();
      queue = Lwt_sequence.create ();
      push }
  in
  start p fd mode;
  extended_handshake p;
  p
