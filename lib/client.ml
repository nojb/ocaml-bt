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

module L    = Log
module Log  = Log.Make (struct let section = "[Client]" end)
module ARC4 = Nocrypto.Cipher_stream.ARC4
module Cs   = Nocrypto.Uncommon.Cs

module IncompleteMetadata = struct
  type data =
    {
      length : int;
      pieces : Bits.t;
      raw : Cstruct.t
    }

  type t = data option ref

  let reset t = t := None

  let metadata_block_size = 1 lsl 14
  let metadata_max_size = 1 lsl 22

  let create () = ref None

  let set_length t length =
    match !t with
    | None ->
        let size = (length + metadata_block_size - 1) / metadata_block_size in
        t := Some {length; pieces = Bits.create size; raw = Cstruct.create length}
    | Some data ->
        if data.length <> length then
          Printf.ksprintf failwith "Metadata size mismatch: %d vs %d" length data.length

  let add m n buf =
    match !m with
    | None -> false
    | Some m ->
        if n < 0 || n >= Bits.length m.pieces then invalid_arg "add";
        Bits.set m.pieces n;
        Cstruct.blit buf 0 m.raw (n * metadata_block_size) (Cstruct.len buf);
        Bits.has_all m.pieces

  let verify m info_hash =
    match !m with
    | None -> None
    | Some m ->
        if not (Bits.has_all m.pieces) then invalid_arg "IncompleteMetadata.verify";
        if SHA1.(equal (digest m.raw) info_hash) then
          Some m.raw
        else
          None

  let iter_missing f m =
    match !m with
    | None -> assert false
    | Some m ->
        for i = 0 to Bits.length m.pieces - 1 do
          if not (Bits.is_set m.pieces i) then
            f i
        done
end

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

  val print : out_channel -> message -> unit

  val writer : message -> Util.W.t

  val max_packet_len : int

  val handle : Cstruct.t -> message list * int

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

  let print oc x =
    let open Printf in
    match x with
    | KEEP_ALIVE -> fprintf oc "keep alive"
    | CHOKE -> fprintf oc "choke"
    | UNCHOKE -> fprintf oc "unchoke"
    | INTERESTED -> fprintf oc "interested"
    | NOT_INTERESTED -> fprintf oc "not interested"
    | HAVE i -> fprintf oc "have %d" i
    | BITFIELD b -> fprintf oc "bitfield with %d/%d pieces" (Bits.count_ones b) (Bits.length b)
    | REQUEST (i, off, len) -> fprintf oc "request %d off:%d len:%d" i off len
    | PIECE (i, off, buf) -> fprintf oc "piece %d off:%d len:%d" i off (Cstruct.len buf)
    | CANCEL (i, off, len) -> fprintf oc "cancel %d off:%d len:%d" i off len
    | PORT port -> fprintf oc "port %d" port
    | HAVE_ALL -> fprintf oc "have all"
    | HAVE_NONE -> fprintf oc "have none"
    | SUGGEST i -> fprintf oc "suggest %d" i
    | REJECT (i, off, len) -> fprintf oc "reject %d off:%d len:%d" i off len
    | ALLOWED pieces -> fprintf oc "allowed %s" (strl string_of_int pieces)
    | EXTENDED (id, _) -> fprintf oc "extended %d" id

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
    | 07 -> PIECE (int cs 1, int cs 5, Util.cs_clone (Cstruct.shift cs 9))
    | 08 -> CANCEL (int cs 1, int cs 5, int cs 9)
    | 09 -> PORT (Cstruct.BE.get_uint16 cs 1)
    | 13 -> SUGGEST (int cs 1)
    | 14 -> HAVE_ALL
    | 15 -> HAVE_NONE
    | 16 -> REJECT (int cs 1, int cs 5, int cs 9)
    | 17 -> ALLOWED (parse_allowed @@ Cstruct.shift cs 1)
    | 20 -> EXTENDED (Cstruct.get_uint8 cs 1, Util.cs_clone (Cstruct.shift cs 2))
    | _  -> failwith "can't parse msg"

  let parse cs =
    if Cstruct.len cs = 0 then
      KEEP_ALIVE
    else
      parse cs

  let max_packet_len = 1 lsl 15 (* 32 * 1024 = 32768 *)

  let handle buf =
    let len = Cstruct.len buf in
    let rec loop off =
      if off + 4 <= len then begin
        let l = Int32.to_int @@ Cstruct.BE.get_uint32 buf off in
        if l < 0 || l > max_packet_len then Printf.kprintf failwith "Wire: invalid packet len (%d)" l;
        if off + 4 + l <= len then
          let msg = parse @@ Cstruct.sub buf (off + 4) l in
          let msgs, rest = loop (off + 4 + l) in
          msg :: msgs, rest
        else
          [], off
      end else
        [], off
    in
    loop 0

end

let listen_backlog = 5
let listen_ports = [50000]
let rechoke_interval = 5.
let optimistic_unchoke_iterations = 2
let rechoke_slots = 10
let block_size = 1 lsl 14 (* 16384 *)
let max_requests = 5

module Piece = struct
  type piece_state =
    | Pending
    | Active of int array
    | Verified

  type piece =
    { mutable state : piece_state;
      length : int;
      offset : int64;
      mutable have : int;
      hash : SHA1.t;
      num_blocks : int }

  let make m i =
    {
      state = Pending;
      length = Metadata.piece_length m i;
      offset = Metadata.offset m i 0;
      have = 0;
      hash = Metadata.hash m i;
      num_blocks = Metadata.block_count m i;
    }

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
end

module Peer = struct
  type addr = Unix.inet_addr * int

  type pex_flags =
    { pex_encryption : bool;
      pex_seed : bool;
      pex_utp : bool;
      pex_holepunch : bool;
      pex_outgoing : bool }

  let keepalive_delay = 20. (* FIXME *)

  type ut_extension =
    | UT_pex
    | UT_metadata

  let string_of_ut_extension = function
    | UT_pex -> "ut_pex"
    | UT_metadata -> "ut_metadata"

  let ut_extension_of_string = function
    | "ut_pex" -> UT_pex
    | "ut_metadata" -> UT_metadata
    | s -> Printf.ksprintf failwith "ut_extension_of_string: %s" s

  type t =
    {
      peer_id : SHA1.t;
      blame                   : Bits.t;
      have                    : Bits.t;
      extbits                 : Bits.t;
      extensions              : (ut_extension, int) Hashtbl.t;
      mutable last_pex        : addr list;
      mutable am_choking      : bool;
      mutable am_interested   : bool;
      mutable peer_choking    : bool;
      mutable peer_interested : bool;
      mutable strikes         : int;
      mutable uploaded        : int64;
      mutable downloaded      : int64;
      mutable last_send : float;
      upload                  : Speedometer.t;
      download                : Speedometer.t;
      requests                : (int * int * int) Lwt_sequence.t;
      peer_requests           : (int * int * int) Lwt_sequence.t;
      send                    : unit Lwt_condition.t;
      queue                   : Wire.message Lwt_sequence.t;
    }

  type incomplete =
    IncompleteMetadata.t

  type complete =
    {
      store : Store.t;
      pieces : Piece.piece array;
      metadata : Metadata.t;
      we_have : Bits.t;
    }

  type swarm_state =
    | Incomplete of incomplete * Cstruct.t Lwt.u
    | Complete of complete

  class type listener =
    object
      method peer_joined : SHA1.t -> Unix.inet_addr -> unit
      method block_received : SHA1.t -> int -> int -> int -> unit
      method piece_verified : int -> unit
    end

  class dummy_listener : listener =
    object
      method peer_joined _ _ = ()
      method block_received _ _ _ _ = ()
      method piece_verified _ = ()
    end

  type swarm =
    {
      id : SHA1.t;
      info_hash : SHA1.t;
      peers : (SHA1.t, t) Hashtbl.t;
      peer_man : PeerMgr.swarm;
      mutable state : swarm_state;
      mutable optimistic_num : int;
      mutable last_choke_unchoke : float;
      mutable listener : listener;
    }

  let has p i =
    if i < 0 then invalid_arg "Peer.has";
    i < Bits.length p.have && Bits.is_set p.have i

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
    if i < 0 then invalid_arg "Peer.worked_on_piece";
    i < Bits.length p.blame && Bits.is_set p.blame i

  let strike p =
    p.strikes <- p.strikes + 1;
    p.strikes

  let send p m =
    ignore (Lwt_sequence.add_r m p.queue);
    Lwt_condition.signal p.send ()

  let reject_metadata_request p piece =
    let id = Hashtbl.find p.extensions UT_metadata in
    let m =
      let d = [ "msg_type", Bcode.Int 2L; "piece", Bcode.Int (Int64.of_int piece) ] in
      Bcode.encode (Bcode.Dict d)
    in
    send p @@ Wire.EXTENDED (id, m)

  let metadata_piece len i data p =
    let id = Hashtbl.find p.extensions UT_metadata in
    let m =
      let d =
        [ "msg_type",   Bcode.Int 1L;
          "piece",      Bcode.Int (Int64.of_int i);
          "total_size", Bcode.Int (Int64.of_int len) ]
      in
      Cs.(Bcode.encode (Bcode.Dict d) <+> data)
    in
    send p @@ Wire.EXTENDED (id, m)

  let got_ut_metadata t p data =
    let m, data = Bcode.decode_partial data in
    let msg_type = Bcode.to_int (Bcode.find "msg_type" m) in
    let piece = Bcode.to_int (Bcode.find "piece" m) in
    match msg_type with
    | 0 -> (* request *)
        begin match t.state with
        | Incomplete _ ->
            reject_metadata_request p piece
        | Complete {metadata; _} ->
            metadata_piece
              (Metadata.length metadata) piece
              (Metadata.get_piece metadata piece) p
        end
    | 1 -> (* data *)
        begin match t.state with
        | Incomplete (m, gotit) ->
            if IncompleteMetadata.add m piece data then begin
              match IncompleteMetadata.verify m t.info_hash with
              | Some raw ->
                  Lwt.wakeup_later gotit raw
                  (* debug "got full metadata"; *)
                  (* let m = Metadata.create (Bcode.decode raw) in *)
                  (* t.state <- Complete (m, *)
                  (* Lwt.return m *)
              | None ->
                  IncompleteMetadata.reset m;
                  Log.error "METADATA HASH CHECK FAILED"
            end
        | Complete _ ->
            ()
        end
    | 2 (* reject *) | _ ->
        ()

  let got_ut_pex t p data =
    (* FIXME support for IPv6 *)
    let m = Bcode.decode data in
    let added = Bcode.find "added" m |> Bcode.to_cstruct in
    let added_f = Bcode.find "added.f" m |> Bcode.to_cstruct in
    let dropped = try Bcode.find "dropped" m |> Bcode.to_cstruct with _ -> Cs.empty in
    let rec loop cs =
      if Cstruct.len cs >= 6 then
        let addr, cs = Cstruct.split cs 6 in
        let ip =
          Unix.inet_addr_of_string
            (Printf.sprintf "%d.%d.%d.%d"
               (Cstruct.get_uint8 addr 0) (Cstruct.get_uint8 addr 1)
               (Cstruct.get_uint8 addr 2) (Cstruct.get_uint8 addr 3))
        in
        let port = Cstruct.BE.get_uint16 addr 4 in
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
    let added = List.combine added added_f in
    let _dropped = loop dropped in
    List.iter (fun (addr, _) -> PeerMgr.add t.peer_man addr) added

  let got_ut_extension = function
    | UT_metadata -> got_ut_metadata
    | UT_pex -> got_ut_pex

  let supported_ut_extensions =
    [ 1, UT_metadata;
      2, UT_pex ]

  (* Outgoing *)

  let send_piece p i o buf =
    p.uploaded <- Int64.add p.uploaded (Int64.of_int @@ Cstruct.len buf);
    Speedometer.add p.upload (Cstruct.len buf);
    (* TODO emit Uploaded event *)
    send p @@ Wire.PIECE (i, o, buf)

  let send_request p i ofs len =
    if p.peer_choking then invalid_arg "Peer.request";
    ignore (Lwt_sequence.add_r (i, ofs, len) p.requests);
    send p @@ Wire.REQUEST (i, ofs, len)

  let extended_handshake p =
    let m =
      List.map (fun (id, name) ->
        string_of_ut_extension name, Bcode.Int (Int64.of_int id)) supported_ut_extensions
    in
    let m = Bcode.Dict ["m", Bcode.Dict m] in
    Log.info "> EXTENDED HANDSHAKE id:%a (%s)" SHA1.print_hex_short p.peer_id
      (String.concat " " (List.map (fun (n, name) ->
           Printf.sprintf "%s %d" (string_of_ut_extension name) n)
           supported_ut_extensions));
    send p @@ Wire.EXTENDED (0, Bcode.encode m)

  let choke p =
    if not p.am_choking then begin
      p.am_choking <- true;
      Lwt_sequence.iter_node_l Lwt_sequence.remove p.peer_requests;
      send p Wire.CHOKE
    end

  let unchoke p =
    if p.am_choking then begin
      p.am_choking <- false;
      send p Wire.UNCHOKE
    end

  let interested p =
    if not p.am_interested then begin
      p.am_interested <- true;
      send p Wire.INTERESTED
    end

  let not_interested p =
    if p.am_interested then begin
      p.am_interested <- false;
      send p Wire.NOT_INTERESTED
    end

  let send_have p i =
    send p @@ Wire.HAVE i

  let send_have_bitfield p bits =
    (* check for redefined length FIXME *)
    Bits.set_length p.have (Bits.length bits);
    Bits.set_length p.blame (Bits.length bits);
    Log.info "> BITFIELD id:%a have:%d total:%d" SHA1.print_hex_short p.peer_id
      (Bits.count_ones bits) (Bits.length bits);
    send p @@ Wire.BITFIELD bits

  let cancel p i o l =
    (* Log.info "> CANCEL id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i o l; *)
    send p @@ Wire.CANCEL (i, o, l);
    try
      let n = Lwt_sequence.find_node_l (fun (i', o', l') -> i = i' && o' = o && l = l') p.requests in
      Lwt_sequence.remove n
    with
    | Not_found ->
        Log.warn "! REQUEST NOT FOUND id:%a idx:%d off:%d len:%d" SHA1.print_hex_short p.peer_id i o l

  let send_port p i =
    send p @@ Wire.PORT i

  let request_metadata_piece p i =
    if i < 0 || not (Hashtbl.mem p.extensions UT_metadata) then invalid_arg "request_metadata_piece";
    let id = Hashtbl.find p.extensions UT_metadata in
    let d =
      [ "msg_type", Bcode.Int 0L;
        "piece", Bcode.Int (Int64.of_int i) ]
    in
    send p @@ Wire.EXTENDED (id, Bcode.encode @@ Bcode.Dict d)

  let send_ut_pex p added dropped =
    let id = Hashtbl.find p.extensions UT_pex in
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
    Log.info "> PEX id:%a added:%d dropped:%d" SHA1.print_hex_short p.peer_id
      (List.length added) (List.length dropped);
    send p @@ Wire.EXTENDED (id, Bcode.encode @@ Bcode.Dict d)

  let send_pex pex p =
    if Hashtbl.mem p.extensions UT_pex then begin
      let added = List.filter (fun a -> not (List.mem a p.last_pex)) pex in
      let dropped = List.filter (fun a -> not (List.mem a pex)) p.last_pex in
      p.last_pex <- pex;
      send_ut_pex p added dropped
    end

  (* Incoming *)

  open Lwt.Infix

  let verify_piece store peers piece =
    if piece.Piece.have = piece.Piece.num_blocks then
      Store.digest store piece.Piece.offset piece.Piece.length >>= fun sha ->
      Lwt.return (SHA1.equal sha piece.Piece.hash)
    else
      Lwt.return false

  let record_block t p i off s =
    match t.state with
    | Complete {we_have; store; pieces} ->
        if i >= 0 && i < Array.length pieces then begin
          let j = off / block_size in
          match pieces.(i).Piece.state with
          | Piece.Pending ->
              Log.warn "Received block #%d for piece #%d, not requested ???" j i
          | Piece.Verified ->
              Log.warn "Received block #%d for piece #%d, already completed" j i
          | Piece.Active parts ->
              let c = parts.(j) in
              if c >= 0 then begin
                parts.(j) <- (-1);
                let do_cancel _ p =
                  if requested p i j (Cstruct.len s) then
                    cancel p i j (Cstruct.len s)
                in
                if c > 1 then Hashtbl.iter do_cancel t.peers;
                pieces.(i).Piece.have <- pieces.(i).Piece.have + 1;
                t.listener # block_received p.peer_id i off (Cstruct.len s);
                Log.info "Received block #%d for piece #%d, have %d, missing %d"
                  j i pieces.(i).Piece.have (Array.length parts - pieces.(i).Piece.have)
              end;
              let off = Int64.(add pieces.(i).Piece.offset (of_int off)) in
              Lwt.async (fun () ->
                Store.write store off s >>= fun () ->
                verify_piece store t.peers pieces.(i) >>= function
                | true ->
                    pieces.(i).Piece.state <- Piece.Verified;
                    Bits.set we_have i;
                    t.listener # piece_verified i;
                    Hashtbl.iter (fun _ p -> send_have p i) t.peers;
                    Lwt.return_unit
                (* maybe update interest ? *)
                | false -> (* FIXME FIXME *)
                    Lwt.return_unit
              )
        end
    | Incomplete _ ->
        ()

  let send_block store pieces p i off len =
    match pieces.(i).Piece.state with
    | Piece.Verified ->
        let off1 = Int64.(add pieces.(i).Piece.offset (of_int off)) in
        Lwt.async (fun () ->
          Store.read store off1 len >|= fun buf ->
          send_piece p i off buf
        )
    | Piece.Pending
    | Piece.Active _ ->
        ()

  let request p pieces =
    if requests p < max_requests then
      match Piece.request_active pieces (has p) with
      | Some (i, j) ->
          let off = j * block_size in
          let len = min block_size (pieces.(i).Piece.length - off) in
          send_request p i off len
      | None ->
          ()

  let request_rejected t reqs =
    match t.state with
    | Complete {pieces; _} ->
        let aux (i, off, _) =
          match pieces.(i).Piece.state with
          | Piece.Verified
          | Piece.Pending -> ()
          | Piece.Active parts ->
              let j = off / block_size in
              if parts.(j) > 0 then parts.(j) <- parts.(j) - 1
        in
        List.iter aux reqs
    | Incomplete _ ->
        ()

  let on_message t p m =
    match m with
    | Wire.KEEP_ALIVE -> ()
    | Wire.CHOKE ->
        if not p.peer_choking then begin
          p.peer_choking <- true;
          let reqs = Lwt_sequence.fold_l (fun r l -> r :: l) p.requests [] in
          Lwt_sequence.iter_node_l (fun n -> Lwt_sequence.remove n) p.requests;
          request_rejected t reqs
        end
    | Wire.UNCHOKE ->
        p.peer_choking <- false
    | Wire.INTERESTED ->
        p.peer_interested <- true
    | Wire.NOT_INTERESTED ->
        p.peer_interested <- false
    | Wire.HAVE i ->
        (* check for got_bitfield_already FIXME *)
        Bits.resize p.have (i + 1);
        Bits.set p.have i
    | Wire.BITFIELD b ->
        (* check for redefinition *)
        Bits.set_length p.have (Bits.length b);
        Bits.blit b 0 p.have 0 (Bits.length b)
        (* Log.info "< BITFIELD id:%s have:%d total:%d" (SHA1.to_hex_short p.id) (Bits.count_ones b) (Bits.length b); *)
    | Wire.REQUEST (i, off, len) ->
        if not p.am_choking then begin
          let (_ : _ Lwt_sequence.node) = Lwt_sequence.add_r (i, off, len) p.peer_requests in
          (* Log.info "< REQUEST id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i off len; *)
          match t.state with
          | Complete {pieces; store; _} ->
              if i >= 0 && i < Array.length pieces then
                send_block store pieces p i off len
              else
                Log.warn "! PEER REQUEST invalid piece:%d" i
          | Incomplete _ ->
              ()
        end
    | Wire.PIECE (i, off, s) ->
        begin match
          Lwt_sequence.find_node_opt_l
            (fun (i', off', len') -> i = i && off = off' && len' = Cstruct.len s)
            p.requests
        with
        | None ->
            Log.warn "! Received peer block #%d of #%d which we were not expecting"
              (off / (16 * 1024)) i
        | Some n ->
            Lwt_sequence.remove n
        end;
        p.downloaded <- Int64.add p.downloaded (Int64.of_int @@ Cstruct.len s);
        Speedometer.add p.download (Cstruct.len s);
        Bits.resize p.blame (i + 1);
        Bits.set p.blame i;
        (* TODO emit Downloaded event *)
        record_block t p i off s
    | Wire.CANCEL (i, off, len) ->
        (* Log.info "< CANCEL id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i off len; *)
        let n =
          Lwt_sequence.find_node_opt_l
            (fun (i', off', len') -> i = i && off = off' && len = len')
            p.peer_requests
        in
        begin match n with
        | Some n ->
            (* FIXME broadcast event *)
            Lwt_sequence.remove n
        | None ->
            Log.warn "! PEER REQUEST NOT FOUND id:%a idx:%d off:%d len:%d"
              SHA1.print_hex_short p.peer_id i off len
        end
    | Wire.EXTENDED (0, s) ->
        let bc = Bcode.decode s in
        let m =
          Bcode.find "m" bc |> Bcode.to_dict |>
          List.map (fun (name, id) -> (name, Bcode.to_int id))
        in
        List.iter (fun (name, id) ->
          try
            let name = ut_extension_of_string name in
            if id = 0 then Hashtbl.remove p.extensions name
            else Hashtbl.replace p.extensions name id
          with
            _ -> ()) m;
        Log.info "< EXTENDED HANDSHAKE id:%a (%s)" SHA1.print_hex_short p.peer_id
          (String.concat " " (List.map (fun (name, n) -> Printf.sprintf "%s %d" name n) m));
        if Hashtbl.mem p.extensions UT_metadata then begin
          match t.state with
          | Incomplete (m, _) ->
              let len = Bcode.find "metadata_size" bc |> Bcode.to_int in
              if len <= IncompleteMetadata.metadata_max_size then begin
                try
                  IncompleteMetadata.set_length m len;
                  IncompleteMetadata.iter_missing (request_metadata_piece p) m;
                with
                | _ ->
                    IncompleteMetadata.reset m;
              end else begin
                Log.warn "! METADATA length %d is too large, ignoring." len;
              end
          | Complete _ ->
              ()
        end
    | Wire.EXTENDED (id, data) ->
        Log.info "< EXTENDED id:%a mid:%d" SHA1.print_hex_short p.peer_id id;
        begin
          try
            let ute = List.assoc id supported_ut_extensions in
            got_ut_extension ute t p data
          with
          | _ -> ()
        end
    | Wire.PORT _ | _ -> ()

  (* Event loop *)

  open Lwt.Infix

  let handle_err t p sock e =
    Log.error "ERROR id:%a exn:%S" SHA1.print_hex_short p.peer_id (Printexc.to_string e);
    let reqs = Lwt_sequence.fold_l (fun r l -> r :: l) p.requests [] in

    request_rejected t reqs;
    PeerMgr.peer_disconnected t.peer_man p.peer_id;
    Hashtbl.remove t.peers p.peer_id;

    Lwt.catch (sock # close) (fun _ -> Lwt.return_unit)

  let reader_loop t p sock =
    let buf = Cstruct.create Wire.max_packet_len in
    let rec loop off =
      Lwt_unix.with_timeout keepalive_delay (fun () -> sock # read (Cstruct.shift buf off)) >>= function
      | 0 ->
          Lwt.fail End_of_file
      | n ->
          let n = n + off in
          let msgs, off = Wire.handle (Cstruct.sub buf 0 n) in
          Cstruct.blit buf off buf 0 (n - off);
          List.iter
            (fun m ->
               Log.debug "[%a] --> %a" SHA1.print_hex_short p.peer_id Wire.print m) msgs;
          List.iter (on_message t p) msgs;
          loop (n - off)
    in
    loop 0

  let writer_loop _t p sock =
    let buf = Cstruct.create Wire.max_packet_len in
    let write m =
      Log.debug "[%a] <-- %a" SHA1.print_hex_short p.peer_id Wire.print m;
      let buf = Util.W.into_cstruct (Wire.writer m) buf in
      Lwt_cstruct.complete (fun buf ->
        sock # write buf >>= fun n ->
        if n > 0 then p.last_send <- Sys.time ();
        Lwt.return n
      ) buf
    in
    let rec loop () =
      Lwt_condition.wait p.send >>= fun () ->
      let rec loop' () =
        match Lwt_sequence.take_opt_l p.queue with
        | Some m ->
            write m >>= loop'
        | None ->
            Lwt.return_unit
      in
      loop' () >>= loop
    in
    loop ()

  let start t p sock =
    Lwt.ignore_result begin
      Lwt.catch
        (fun () -> Lwt.pick [reader_loop t p sock; writer_loop t p sock])
        (handle_err t p sock)
    end

  let create_peer peer_id =
    {
      peer_id;
      am_choking = true;
      am_interested = false;
      peer_choking = true;
      peer_interested = false;
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
      last_send = min_float;
    }

  let welcome t sock exts id =
    let p = create_peer id in
    (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)
    start t p sock;
    extended_handshake p;
    begin match t.state with
    | Complete {we_have; _} ->
        send_have_bitfield p we_have
    | Incomplete _ ->
        ()
    end;
    p

  let connect t addr =
    let push = function
      | Handshake.Ok (sock, ext, peer_id) ->
          Log.info "Connected to %s:%d [%a] successfully" (Unix.string_of_inet_addr (fst addr)) (snd addr)
            SHA1.print_hex_short peer_id;
          let p = welcome t sock ext peer_id in
          t.listener # peer_joined peer_id (fst addr);
          Hashtbl.add t.peers peer_id p;
          PeerMgr.handshake_ok t.peer_man addr peer_id
      | Handshake.Failed ->
          Log.error "Connection to %s:%d failed" (Unix.string_of_inet_addr (fst addr)) (snd addr);
          PeerMgr.handshake_failed t.peer_man addr
    in
    Handshake.outgoing ~id:t.id ~info_hash:t.info_hash addr push

  let rechoke_compare (p1, salt1) (p2, salt2) =
    if download_speed p1 <> download_speed p2 then
      compare (download_speed p2) (download_speed p1)
    else
    if upload_speed p1 <> upload_speed p2 then
      compare (upload_speed p2) (upload_speed p1)
    else
    if p1.am_choking <> p2.am_choking then
      compare p1.am_choking p2.am_choking
    else
      compare salt1 salt2

  let update_choke_unchoke t =
    let now = Unix.time () in
    if t.last_choke_unchoke +. rechoke_interval >= now then begin
      t.last_choke_unchoke <- now;
      t.optimistic_num <-
        if t.optimistic_num = 0 then optimistic_unchoke_iterations else t.optimistic_num - 1;
      let wires =
        let add _ p wires = (p, Random.int (1 lsl 29)) :: wires in
        Hashtbl.fold add t.peers []
      in
      let wires = List.sort rechoke_compare wires in
      let rec select n acc = function
        | (p, _) :: wires when n < rechoke_slots ->
            let n = if p.peer_interested then n + 1 else n in
            select n (p :: acc) wires
        | wires ->
            let wires = List.filter (fun (p, _) -> p.peer_interested) wires in
            if t.optimistic_num = 0 && List.length wires > 0 then
              let p, _ = List.nth wires (Random.int (List.length wires)) in
              (p :: acc), wires
            else
              acc, wires
      in
      let to_unchoke, to_choke = select 0 [] wires in
      List.iter (fun p -> unchoke p) to_unchoke;
      List.iter (fun (p, _) -> choke p) to_choke
    end

  let update_requests t =
    match t.state with
    | Complete {pieces; _} ->
        Hashtbl.iter (fun _ p -> if not p.peer_choking then request p pieces) t.peers
    | Incomplete _ ->
        ()

  let update_interest t =
    match t.state with
    | Complete {pieces; _} ->
        let rec loop i p =
          if i < Array.length pieces then
            if has p i then
              match pieces.(i).Piece.state with
              | Piece.Active _
              | Piece.Pending ->
                  interested p
              | Piece.Verified ->
                  loop (i + 1) p
            else
              loop (i + 1) p
          else
            not_interested p
        in
        Hashtbl.iter (fun _ p -> loop 0 p) t.peers
    | Incomplete _ ->
        ()

  let send_keep_alives t =
    let now = Unix.time () in
    let aux _ p =
      if p.last_send +. keepalive_delay >= now then
        send p Wire.KEEP_ALIVE
    in
    Hashtbl.iter aux t.peers

  let the_loop t =
    let rec malthusian_process () =
      update_interest t;
      update_requests t;
      update_choke_unchoke t;
      send_keep_alives t;
      Lwt_unix.sleep 1.0 >>= malthusian_process
    in
    malthusian_process ()

  let load_torrent bt m =
    Store.create (Metadata.files m) >>= fun store ->
    let pieces = Array.init (Metadata.piece_count m) (Piece.make m) in
    let have = Bits.create (Metadata.piece_count m) in
    let rec loop i =
      if i < Metadata.piece_count m then begin
        Store.digest store pieces.(i).Piece.offset pieces.(i).Piece.length >>= fun sha ->
        if SHA1.equal sha pieces.(i).Piece.hash then begin
          pieces.(i).Piece.state <- Piece.Verified;
          Bits.set have i
        end;
        loop (i + 1)
      end else
        Lwt.return (store, pieces, have)
    in
    loop 0

  let create_swarm id info_hash =
    let t, u = Lwt.wait () in
    let rec swarm =
      lazy
        {
          id;
          info_hash;
          peers = Hashtbl.create 0;
          peer_man = Lazy.force peer_man;
          state = Incomplete (IncompleteMetadata.create (), u);
          optimistic_num = optimistic_unchoke_iterations;
          last_choke_unchoke = min_float;
          listener = new dummy_listener;
        }
    and peer_man =
      lazy
        (PeerMgr.create (fun addr timeout ->
            Lwt.ignore_result (
              Lwt_unix.sleep timeout >|= fun () ->
              connect (Lazy.force swarm) addr)
          )
        )
    in
    Lazy.force swarm

  class client info_hash =
    let id = SHA1.generate ~prefix:"OCAML" () in
    let sw = create_swarm id info_hash in
    object (client)
      inherit dummy_listener

      method start =
        Lwt.ignore_result (Lwt.wrap1 the_loop sw)

      initializer
        sw.listener <- (client :> listener)
    end
end

open Lwt.Infix

type addr = Unix.inet_addr * int

(* let pex_delay = 60.0 *)

(* let rec pex_pulse pm = *)
(*   let pex = Hashtbl.fold (fun addr _ l -> addr :: l) pm.peers [] in *)
(*   iter_peers (fun p -> Peer.send_pex p pex) pm; *)
(*   Lwt_unix.sleep pex_delay >>= fun () -> pex_pulse pm *)

let start_server ?(port = 0) push =
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
  let port = match Lwt_unix.getsockname fd with Unix.ADDR_INET (_, p) -> p | Unix.ADDR_UNIX _ -> assert false in
  Lwt_unix.listen fd listen_backlog;
  Log.debug "listening on port %u" port;
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, sa) ->
    Log.debug "accepted connection from %s" (Util.string_of_sockaddr sa);
    let addr = match sa with Unix.ADDR_INET (ip, p) -> ip, p | Unix.ADDR_UNIX _ -> assert false in
    push addr (new Util.tcp fd);
    loop ()
  in
  loop ()

let start_server ?port () =
  let push addr sock =
    (* FIXME *)
    ()
  in
  Lwt.ignore_result (start_server ?port push)

let start_trackers {Peer.id; info_hash; peer_man; _} trackers =
  let rec aux tracker =
    Tracker.announce ~info_hash id tracker >>= fun (interval, addrs) ->
    List.iter (PeerMgr.add peer_man) addrs;
    Lwt_unix.sleep interval >>= fun () ->
    aux tracker
  in
  Lwt.ignore_result (Lwt_list.iter_p aux trackers)

let start t trackers =
  start_server ();
  start_trackers t trackers

(* let create mg = *)
(*   (\* let ch, push = let ch, push = Lwt_stream.create () in ch, (fun x -> push (Some x)) in *\) *)
(*   let id = SHA1.generate ~prefix:"OCAML" () in *)
(*   let info_hash = mg.Magnet.xt in *)
(*   let sw = Peer.create id info_hash in *)
(*   start sw mg.Magnet.tr; *)
(*   sw *)
