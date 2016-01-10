(* The MIT License (MIT)

   Copyright (c) 2013-2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

module Test = struct

  let block_size = 1 lsl 14 (* 16384 *)
  let metadata_max_size = 1 lsl 22
  let metadata_block_size = 1 lsl 14

  module Int = struct
    type t = int
    let compare : int -> int -> int = compare
  end

  module IntMap = Map.Make (Int)

  module Bitv : sig
    type t
    val empty : t
    val single : int -> t
    val set : int -> t -> t
    val unset : int -> t -> t
    val is_set : int -> t -> bool
    val count : t -> int
  end = struct
    module IntSet = Set.Make (Int)
    type t = IntSet.t
    let empty = IntSet.empty
    let single m = IntSet.singleton m
    let set m v = IntSet.add m v
    let unset m v = IntSet.remove m v
    let is_set m v = IntSet.mem m v
    let count v = IntSet.cardinal v
  end

  (* type piece = *)
  (*   { *)
  (*     state : piece_state; *)
  (*     length : int; *)
  (*     offset : int64; *)
  (*     have : Bitv.t; *)
  (*     hash : SHA1.t; *)
  (*     num_blocks : int; *)
  (*   } *)

  module SHA1Map = Map.Make (SHA1)

  module Piece = struct
    type block =
      | Done
      | Waiting of SHA1.t list
    type state =
      | Pending
      | Active of block IntMap.t
      | Waiting
      | Verified
    let length m i = Metadata.piece_length m i
    let offset m i = Metadata.offset m i 0
    let hash m i = Metadata.hash m i
  end

  type peer =
    {
      am_interested : bool;
      interested_in_us : bool;
      am_choking : bool;
      choking_us : bool;
    }

  type metadata = Metadata.t
    (* { *)
    (*   name : string; *)
    (*   info_hash : SHA1.t; *)
    (*   piece_length : int; *)
    (*   total_length : int64; *)
    (* } *)

  type getting =
    {
      length : int;
      num_pieces : int;
      have : string IntMap.t;
    }

  type loading =
    {
      info : metadata;
      have : Bitv.t;
    }

  type sharing =
    {
      last_rechoke : float;
      info : metadata;
      have : Bitv.t;
      in_progress : Bitv.t;
      pieces : Piece.state IntMap.t;
      num_pieces : int;
      have_pieces : int;
    }

  type state =
    | Waiting
    | Getting of getting
    | Loading of loading
    | Sharing of sharing

  type context =
    {
      info_hash : SHA1.t;
      peers : peer SHA1Map.t;
      state : state;
    }

  type event =
    | PeerConnected of SHA1.t
    | PeerLeft of SHA1.t
    | BlockReceived of SHA1.t * int * int * string
    | DigestComputed of int * SHA1.t
    | BlockRequested of SHA1.t * int * int * int
    | MetadataLengthReceived of SHA1.t * int
    | MetadataBlockReceived of SHA1.t * int * string
    | MetadataBlockRequested of SHA1.t * int
    | PeerChoked of SHA1.t
    | PeerInterested of SHA1.t
    | PeerHas of SHA1.t * string
    | TorrentOpened of Bitv.t

  type action =
    | SendHave of SHA1.t * Bitv.t
    | SendBlock of SHA1.t * int * int * int
    | WriteBlock of int * int * string
    | ComputeDigest of int
    | CancelBlock of SHA1.t * int * int * int
    | SendMetadataBlock of SHA1.t * int * int * Cstruct.t
    | ChokePeer of SHA1.t
    | InterestingPeer of SHA1.t
    | RequestBlock of SHA1.t * int * int * int
    | RequestMetadataBlock of SHA1.t * int
    | RejectMetadataBlockRequest of SHA1.t * int
    | OpenTorrent of metadata

  let num_pieces_have pieces =
    IntMap.fold (fun _ st n ->
      match st with Piece.Verified | Piece.Waiting -> n + 1 | _ -> n
    ) pieces 0

  let handle ev st =
    match ev, st.state with
    | PeerConnected id, Sharing {have; _} ->
        let p = assert false in
        let st = {st with peers = SHA1Map.add id p st.peers} in
        st, [SendHave (id, have)]
    | BlockReceived (id, idx, off, data), Sharing sh ->
        let {pieces; info; num_pieces; _} = sh in
        let j = off / block_size in
        let pieces, actions =
          match IntMap.find idx pieces with
          | Piece.Pending
          | Piece.Verified
          | Piece.Waiting ->
              pieces, []
          | Piece.Active parts ->
              begin match IntMap.find j parts with
              | Piece.Waiting peers ->
                  let parts = IntMap.add j Piece.Done parts in
                  let peers = List.filter (fun id' -> id <> id) peers in
                  let cancels =
                    List.map (fun id -> CancelBlock (id, idx, off, String.length data)) peers
                  in
                  (* let off = Int64.(add (Piece.offset info idx) (of_int off)) in *)
                  let pieces, actions =
                    if num_pieces_have pieces = num_pieces then
                      let pieces = IntMap.add idx Piece.Waiting pieces in
                      pieces, [WriteBlock (idx, off, data); ComputeDigest idx] (* FIXME *)
                    else
                      let pieces = IntMap.add idx (Piece.Active parts) pieces in
                      pieces, [WriteBlock (idx, off, data)]
                  in
                  pieces, (actions @ cancels)
              | Piece.Done ->
                  pieces, []
              | exception Not_found ->
                  pieces, []
              end
        in
        let st = {st with state = Sharing {sh with pieces}} in
        st, actions
    | DigestComputed (idx, hash), Sharing sh -> (* idx -> off, len ---> deduce idx *)
        let {pieces; have; info; _} = sh in
        if hash = Piece.hash info idx then (* FIXME *)
          let pieces = IntMap.add idx Piece.Verified pieces in
          let have = Bitv.set idx have in
          let actions =
            SHA1Map.fold (fun id _ l -> SendHave (id, Bitv.single idx) :: l) st.peers []
          in
          let sh = {sh with pieces; have} in
          {st with state = Sharing sh}, actions
          (* maybe update interest ? *)
        else
          st, [] (* FIXME *)
    | BlockRequested (id, idx, off, len), Sharing sh ->
        let {pieces; info; _} = sh in
        let actions =
          match IntMap.find idx pieces with
          | Piece.Verified ->
              (* let off1 = Int64.(add (Piece.offset info idx) (of_int off)) in *)
              [SendBlock (id, idx, off, len)]
          | Piece.Pending
          | Piece.Waiting
          | Piece.Active _ ->
              []
        in
        st, actions
    | MetadataLengthReceived (id, len), Waiting ->
        let num_pieces = (len + metadata_block_size - 1) / metadata_block_size in
        let get =
          {
            length = len;
            num_pieces;
            have = IntMap.empty;
          }
        in
        let rec span i = if i >= num_pieces then [] else i :: span (i+1) in
        let actions = List.map (fun i -> RequestMetadataBlock (id, i)) (span num_pieces) in
        {st with state = Getting get}, actions
    | MetadataLengthReceived (id, len), Getting get ->
        if len <= metadata_max_size then
          if get.length <> len then
            {st with state = Waiting}, []
          else
            let actions =
              let rec loop i =
                if i >= get.num_pieces then []
                else if IntMap.mem i get.have then
                  loop (i+1)
                else
                  RequestMetadataBlock (id, i) :: loop (i + 1)
              in
              loop 0
            in
            st, actions
        else
          st, []
          (* Lwt_log.ign_warning_f "METADATA length %d is too large, ignoring." len *)
    | PeerChoked id, Sharing _ ->
        begin match SHA1Map.find id st.peers with
        | {choking_us = false; _} as p ->
            let peers = SHA1Map.add id {p with choking_us = true} st.peers in
            (* let reqs = Lwt_sequence.fold_l (fun r l -> r :: l) requests [] in *)
            (* Lwt_sequence.iter_node_l (fun n -> Lwt_sequence.remove n) requests; *)
            (* let aux (i, off, _) = *)
            (*   match pieces.(i).Piece.state with *)
            (*   | Piece.Verified *)
            (*   | Piece.Pending -> () *)
            (*   | Piece.Active parts -> *)
            (*       let j = off / block_size in *)
            (*       if parts.(j) > 0 then parts.(j) <- parts.(j) - 1 *)
            (* in *)
            (* List.iter aux reqs *)
            {st with peers}, []
        | {choking_us = true; _} ->
            st, []
        | exception Not_found ->
            st, []
        end
    | MetadataBlockRequested (id, idx), Sharing sh ->
        let {info; _} = sh in
        st, [SendMetadataBlock (id, idx, Metadata.length info, Metadata.get_piece info idx)]
    | MetadataBlockRequested (id, idx), Waiting
    | MetadataBlockRequested (id, idx), Getting _ ->
        st, [RejectMetadataBlockRequest (id, idx)]
    | MetadataBlockReceived (id, idx, data), Getting get ->
        let have = IntMap.add idx data get.have in
        if IntMap.cardinal get.have = get.num_pieces then
          let raw = Bytes.create get.length in
          IntMap.iter (fun i d ->
            Bytes.blit d 0 raw (i * metadata_block_size) (String.length d)
          ) have;
          if SHA1.digest (Cstruct.of_string raw) = st.info_hash then
            let info = Metadata.create (Bcode.decode (Cstruct.of_string raw)) in
            let load = {info; have = Bitv.empty} in
            {st with state = Loading load}, [OpenTorrent info; ComputeDigest 0] (* FIXME *)
          else
            {st with state = Waiting}, []
        else
          st, []
    | DigestComputed (idx, hash), Loading load ->
        (* FIXME check for done *)
        if hash = Metadata.hash load.info idx then
          let have = Bitv.set idx load.have in
          let actions =
            if idx >= Metadata.piece_count load.info then
              []
            else
              [ComputeDigest (idx+1)]
          in
          {st with state = Loading {load with have}}, actions
        else
          st, []
end

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
  val sprint : unit -> message -> string

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

  let sprint () x =
    let open Printf in
    match x with
    | KEEP_ALIVE -> "keep-alive"
    | CHOKE -> "choke"
    | UNCHOKE -> "unchoke"
    | INTERESTED -> "interested"
    | NOT_INTERESTED -> "not interested"
    | HAVE i -> sprintf "have %d" i
    | BITFIELD b -> sprintf "bitfield with %d/%d pieces" (Bits.count_ones b) (Bits.length b)
    | REQUEST (i, off, len) -> sprintf "request %d %d %d" i off len
    | PIECE (i, off, buf) -> sprintf "piece %d %d %d" i off (Cstruct.len buf)
    | CANCEL (i, off, len) -> sprintf "cancel %d %d %d" i off len
    | PORT i -> sprintf "port %d" i
    | HAVE_ALL -> "have-all"
    | HAVE_NONE -> "have-none"
    | SUGGEST i -> sprintf "suggest %d" i
    | REJECT (i, off, len) -> sprintf "reject %d off:%d len:%d" i off len
    | ALLOWED pieces -> sprintf "allowed %s" (strl string_of_int pieces)
    | EXTENDED (id, _) -> sprintf "extended %d" id

  let print oc x =
    output_string oc (sprint () x)

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
                  Lwt_log.ign_info_f "Requesting ENDGAME block #%d of #%d" j i;
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
                Lwt_log.ign_debug_f "Requesting PENDING block #%d of #%d" j i;
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
                  Lwt_log.ign_debug_f "Requesting ACTIVE block #%d of #%d" j i;
                  Some (i, j)
            else
              loop (i + 1)
    in
    loop 0
end

type ut_extension =
  | UT_pex
  | UT_metadata

let string_of_ut_extension = function
  | UT_pex -> "ut_pex"
  | UT_metadata -> "ut_metadata"

let supported_ut_extensions =
  [ 1, UT_metadata;
    2, UT_pex ]

let ut_extension_of_string = function
  | "ut_pex" -> UT_pex
  | "ut_metadata" -> UT_metadata
  | s -> Printf.ksprintf failwith "ut_extension_of_string: %s" s

type pex_flags =
  {
    pex_encryption : bool;
    pex_seed : bool;
    pex_utp : bool;
    pex_holepunch : bool;
    pex_outgoing : bool;
  }

let keepalive_delay = 20. (* FIXME *)

type addr = Unix.inet_addr * int

module Peer = struct

  class peer ~id ~sock ~client =
    object (peer)
      val mutable am_choking = true
      val mutable am_interested = false
      val mutable choking_us = true
      val mutable interested_in_us = false
      val mutable last_send = min_float
      val mutable strikes = 0
      val mutable uploaded = 0L
      val mutable downloaded = 0L
      val mutable last_pex = []
      val download = Speedometer.create ()
      val upload = Speedometer.create ()
      val extensions = Hashtbl.create 0
      val blame = Bits.create 0
      val have = Bits.create 0
      val requests = Lwt_sequence.create ()
      val peer_requests = Lwt_sequence.create ()
      val send = Lwt_condition.create ()
      val queue = Lwt_sequence.create ()

      method id =
        id

      method am_choking =
        am_choking

      method interested_in_us =
        interested_in_us

      method choking_us =
        choking_us

      method send_port i =
        peer # send (Wire.PORT i)

      method send_have i =
        peer # send (Wire.HAVE i)

      method send_choke =
        if not am_choking then begin
          am_choking <- true;
          Lwt_sequence.iter_node_l Lwt_sequence.remove peer_requests;
          peer # send Wire.CHOKE
        end

      method send_unchoke =
        if am_choking then begin
          am_choking <- false;
          peer # send Wire.UNCHOKE
        end

      method send_interested =
        if not am_interested then begin
          am_interested <- true;
          peer # send Wire.INTERESTED
        end

      method send_not_interested =
        if am_interested then begin
          am_interested <- false;
          peer # send Wire.NOT_INTERESTED
        end

      method send_metadata_request i =
        if i < 0 || not (Hashtbl.mem extensions UT_metadata) then invalid_arg "request_metadata_piece";
        let id = Hashtbl.find extensions UT_metadata in
        let d =
          [
            "msg_type", Bcode.Int 0L;
            "piece", Bcode.Int (Int64.of_int i);
          ]
        in
        peer # send (Wire.EXTENDED (id, Bcode.encode @@ Bcode.Dict d))

      method has i =
        0 <= i && i < Bits.length have && Bits.is_set have i

      method download_speed =
        Speedometer.speed download

      method upload_speed =
        Speedometer.speed upload

      method num_requests =
        Lwt_sequence.length requests

      method requested i off len =
        match Lwt_sequence.find_node_opt_l (fun (i', off', len') -> i = i' && off = off' && len = len') requests with
        | None ->
            false
        | Some _ ->
            true

      method worked_on_piece i =
        0 <= i && i < Bits.length blame && Bits.is_set blame i

      method strike =
        strikes <- strikes + 1;
        strikes

      method private send m =
        ignore (Lwt_sequence.add_r m queue);
        Lwt_condition.signal send ()

      method send_keep_alive =
        let now = Unix.time () in
        if last_send +. keepalive_delay >= now then
          peer # send Wire.KEEP_ALIVE

      method send_metadata_rejection piece =
        let id = Hashtbl.find extensions UT_metadata in
        let m =
          let d =
            [
              "msg_type", Bcode.Int 2L;
              "piece", Bcode.Int (Int64.of_int piece);
            ]
          in
          Bcode.encode (Bcode.Dict d)
        in
        peer # send (Wire.EXTENDED (id, m))

      method send_metadata_piece len i data =
        let id = Hashtbl.find extensions UT_metadata in
        let m =
          let d =
            [ "msg_type",   Bcode.Int 1L;
              "piece",      Bcode.Int (Int64.of_int i);
              "total_size", Bcode.Int (Int64.of_int len) ]
          in
          Cs.(Bcode.encode (Bcode.Dict d) <+> data)
        in
        peer # send (Wire.EXTENDED (id, m))

      method send_piece i o buf =
        uploaded <- Int64.add uploaded (Int64.of_int @@ Cstruct.len buf);
        Speedometer.add upload (Cstruct.len buf);
        (* TODO emit Uploaded event *)
        peer # send (Wire.PIECE (i, o, buf))

      method send_request i ofs len =
        if choking_us then invalid_arg "Peer.request";
        ignore (Lwt_sequence.add_r (i, ofs, len) requests);
        peer # send (Wire.REQUEST (i, ofs, len))

      method send_extended_handshake =
        let m =
          List.map (fun (id, name) ->
            string_of_ut_extension name, Bcode.Int (Int64.of_int id)) supported_ut_extensions
        in
        let m = Bcode.Dict ["m", Bcode.Dict m] in
        Lwt_log.ign_debug_f "> EXTENDED HANDSHAKE id:%a (%s)" SHA1.sprint_hex_short id
          (String.concat " " (List.map (fun (n, name) ->
               Printf.sprintf "%s %d" (string_of_ut_extension name) n)
               supported_ut_extensions));
        peer # send (Wire.EXTENDED (0, Bcode.encode m))

      method send_have_bitfield bits =
        (* check for redefined length FIXME *)
        Bits.set_length have (Bits.length bits);
        Bits.set_length blame (Bits.length bits);
        Lwt_log.ign_debug_f "> BITFIELD id:%a have:%d total:%d" SHA1.sprint_hex_short id
          (Bits.count_ones bits) (Bits.length bits);
        peer # send (Wire.BITFIELD bits)

      method send_cancel i o l =
        (* Log.info "> CANCEL id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i o l; *)
        peer # send (Wire.CANCEL (i, o, l));
        try
          let n = Lwt_sequence.find_node_l (fun (i', o', l') -> i = i' && o' = o && l = l') requests in
          Lwt_sequence.remove n
        with
        | Not_found ->
            Lwt_log.ign_warning_f "! REQUEST NOT FOUND id:%a idx:%d off:%d len:%d" SHA1.sprint_hex_short id i o l

      method private send_ut_pex added dropped =
        let eid = Hashtbl.find extensions UT_pex in
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
          [
            "added", Bcode.String (c added);
            "added.f", Bcode.String (Cs.create_with (List.length added) 0);
            "dropped", Bcode.String (c dropped);
          ]
        in
        Lwt_log.ign_debug_f "> PEX id:%a added:%d dropped:%d" SHA1.sprint_hex_short id
          (List.length added) (List.length dropped);
        peer # send (Wire.EXTENDED (eid, Bcode.encode @@ Bcode.Dict d))

      method send_pex pex =
        if Hashtbl.mem extensions UT_pex then begin
          let added = List.filter (fun a -> not (List.mem a last_pex)) pex in
          let dropped = List.filter (fun a -> not (List.mem a pex)) last_pex in
          last_pex <- pex;
          peer # send_ut_pex added dropped
        end

      method private got_ut_metadata data =
        let m, data = Bcode.decode_partial data in
        let msg_type = Bcode.to_int (Bcode.find "msg_type" m) in
        let piece = Bcode.to_int (Bcode.find "piece" m) in
        match msg_type with
        | 0 -> (* request *)
            client # got_metadata_request peer piece
        | 1 -> (* data *)
            client # got_metadata_piece peer piece data
        | 2 (* reject *) | _ ->
            ()

      method private got_ut_pex data =
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
          {
            pex_encryption = n land 0x1 <> 0;
            pex_seed = n land 0x2 <> 0;
            pex_utp = n land 0x4 <> 0;
            pex_holepunch = n land 0x8 <> 0;
            pex_outgoing = n land 0x10 <> 0;
          }
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
        client # got_pex added

      method private got_ut_extension = function
        | UT_metadata -> peer # got_ut_metadata
        | UT_pex -> peer # got_ut_pex

      method private got_message m =
        match m with
        | Wire.KEEP_ALIVE ->
            ()
        | Wire.CHOKE ->
            if not choking_us then begin
              choking_us <- true;
              let reqs = Lwt_sequence.fold_l (fun r l -> r :: l) requests [] in
              Lwt_sequence.iter_node_l (fun n -> Lwt_sequence.remove n) requests;
              client # got_choked peer reqs
            end
        | Wire.UNCHOKE ->
            choking_us <- false
        | Wire.INTERESTED ->
            interested_in_us <- true
        | Wire.NOT_INTERESTED ->
            interested_in_us <- false
        | Wire.HAVE i ->
            (* check for got_bitfield_already FIXME *)
            Bits.resize have (i + 1);
            Bits.set have i
        | Wire.BITFIELD b ->
            (* check for redefinition *)
            Bits.set_length have (Bits.length b);
            Bits.blit b 0 have 0 (Bits.length b)
        (* Log.info "< BITFIELD id:%s have:%d total:%d" (SHA1.to_hex_short p.id) (Bits.count_ones b) (Bits.length b); *)
        | Wire.REQUEST (i, off, len) ->
            if not am_choking then begin
              let (_ : _ Lwt_sequence.node) = Lwt_sequence.add_r (i, off, len) peer_requests in
              (* Log.info "< REQUEST id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i off len; *)
              client # got_request peer i off len
            end
        | Wire.PIECE (i, off, s) ->
            begin match
              Lwt_sequence.find_node_opt_l
                (fun (i', off', len') -> i = i' && off = off' && len' = Cstruct.len s)
                requests
            with
            | None ->
                Lwt_log.ign_warning_f "Received peer block #%d of #%d which we were not expecting"
                  (off / (16 * 1024)) i
            | Some n ->
                Lwt_sequence.remove n
            end;
            downloaded <- Int64.add downloaded (Int64.of_int @@ Cstruct.len s);
            Speedometer.add download (Cstruct.len s);
            Bits.resize blame (i + 1);
            Bits.set blame i;
            (* TODO emit Downloaded event *)
            client # got_block peer i off s
        | Wire.CANCEL (i, off, len) ->
            (* Log.info "< CANCEL id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i off len; *)
            let n =
              Lwt_sequence.find_node_opt_l
                (fun (i', off', len') -> i = i' && off = off' && len = len')
                peer_requests
            in
            begin match n with
            | Some n ->
                (* FIXME broadcast event *)
                Lwt_sequence.remove n
            | None ->
                Lwt_log.ign_warning_f "PEER REQUEST NOT FOUND id:%a idx:%d off:%d len:%d"
                  SHA1.sprint_hex_short id i off len
            end
        | Wire.EXTENDED (0, s) ->
            let bc = Bcode.decode s in
            let m =
              Bcode.find "m" bc |> Bcode.to_dict |> List.map (fun (name, id) -> (name, Bcode.to_int id))
            in
            List.iter (fun (name, id) ->
              try
                let name = ut_extension_of_string name in
                if id = 0 then
                  Hashtbl.remove extensions name
                else
                  Hashtbl.replace extensions name id
              with _ ->
                ()
            ) m;
            Lwt_log.ign_info_f "< EXTENDED HANDSHAKE id:%a (%s)" SHA1.sprint_hex_short id
              (String.concat " " (List.map (fun (name, n) -> Printf.sprintf "%s %d" name n) m));
            if Hashtbl.mem extensions UT_metadata then begin
              let len = Bcode.find "metadata_size" bc |> Bcode.to_int in
              client # got_metadata_size peer len
            end
        | Wire.EXTENDED (eid, data) ->
            Lwt_log.ign_info_f "< EXTENDED id:%a mid:%d" SHA1.sprint_hex_short id eid;
            if List.mem_assoc eid supported_ut_extensions then begin
              let ute = List.assoc eid supported_ut_extensions in
              peer # got_ut_extension ute data
            end
        | Wire.PORT _ ->
            ()

      method private handle_err e =
        Lwt_log.ign_error_f "ERROR id:%a exn:%S" SHA1.sprint_hex_short id (Printexc.to_string e);
        let reqs = Lwt_sequence.fold_l (fun r l -> r :: l) requests [] in
        client # got_choked peer reqs;
        client # peer_disconnected peer;
        Lwt.catch (sock # close) (fun _ -> Lwt.return_unit)

      method private reader_loop =
        let open Lwt.Infix in
        let buf = Cstruct.create Wire.max_packet_len in
        let rec loop off =
          Lwt_unix.with_timeout keepalive_delay (fun () -> sock # read (Cstruct.shift buf off)) >>= function
          | 0 ->
              Lwt.fail End_of_file
          | n ->
              let n = n + off in
              let msgs, off = Wire.handle (Cstruct.sub buf 0 n) in
              Cstruct.blit buf off buf 0 (n - off);
              List.iter (fun m ->
                Lwt_log.ign_debug_f "[%a] --> %a" SHA1.sprint_hex_short id Wire.sprint m
              ) msgs;
              List.iter (peer # got_message) msgs;
              loop (n - off)
        in
        loop 0

      method private writer_loop =
        let open Lwt.Infix in
        let buf = Cstruct.create Wire.max_packet_len in
        let write m =
          Lwt_log.ign_debug_f "[%a] <-- %a" SHA1.sprint_hex_short id Wire.sprint m;
          let buf = Util.W.into_cstruct (Wire.writer m) buf in
          Lwt_cstruct.complete (fun buf ->
            sock # write buf >>= fun n ->
            if n > 0 then last_send <- Sys.time ();
            Lwt.return n
          ) buf
        in
        let rec loop () =
          Lwt_condition.wait send >>= fun () ->
          let rec loop' () =
            match Lwt_sequence.take_opt_l queue with
            | Some m ->
                write m >>= loop'
            | None ->
                Lwt.return_unit
          in
          loop' () >>= loop
        in
        loop ()

      initializer
        Lwt.ignore_result begin
          Lwt.catch
            (fun () -> Lwt.pick [peer # reader_loop; peer # writer_loop])
            (peer # handle_err)
        end
    end
end

let rechoke_compare (p1, salt1) (p2, salt2) =
  if p1 # download_speed <> p2 # download_speed then
    compare (p2 # download_speed) (p1 # download_speed)
  else
  if p1 # upload_speed <> p2 # upload_speed then
    compare (p2 # upload_speed) (p1 # upload_speed)
  else
  if p1 # am_choking <> p2 # am_choking then
    compare (p1 # am_choking) (p2 # am_choking)
  else
    compare salt1 salt2

module Client = struct

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

  open Lwt.Infix

  let verify_piece store peers piece =
    if piece.Piece.have = piece.Piece.num_blocks then
      Store.digest store piece.Piece.offset piece.Piece.length >>= fun sha ->
      Lwt.return (SHA1.equal sha piece.Piece.hash)
    else
      Lwt.return false

  let load_torrent m =
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

  type peer =
    {
      mutable reconnect : bool;
      mutable retries : int;
      mutable timeout : float;
    }

  module A = struct type t = addr let equal a1 a2 = compare a1 a2 = 0 let hash = Hashtbl.hash end
  module H = Hashtbl.Make (A)

  let default_size = 50

  class type connecter =
    object
      method connect : addr -> unit
    end

  class peer_manager ?(size = default_size) () =
    let reconnect_wait = [1.; 5.; 15.; 30.; 60.; 120.; 300.; 600.] in
    object (mgr)
      val wires = Hashtbl.create 0
      val connections = H.create 0
      val queue = Queue.create ()
      val peers = H.create 0
      val mutable client = object method connect _ = () end

      method set_client cl =
        client <- cl

      method private drain =
        (* Log.debug "[PeerMgr.drain] length:%d connected:%d" (H.length sw.connections) (Hashtbl.length sw.wires); *)
        if H.length connections < size then
          match try Some (Queue.pop queue) with Queue.Empty -> None with
          | None ->
              ()
          | Some addr ->
              let p = H.find peers addr in
              H.add connections addr ();
              Lwt.ignore_result (Lwt_unix.sleep p.timeout >|= fun () -> client # connect addr)

      method add addr =
        if not (H.mem peers addr) then begin
          (* Log.debug "SWARM ADD addr:%s port:%d" (Unix.string_of_inet_addr (fst addr)) (snd addr); *)
          H.add peers addr {reconnect = false; retries = 0; timeout = List.hd reconnect_wait};
          Queue.push addr queue;
          mgr # drain
        end

      method remove addr =
        let p = H.find peers addr in
        if not p.reconnect || p.retries >= List.length reconnect_wait then begin
          (* Log.debug "SWARM REMOVE addr:%s port:%d" (Unix.string_of_inet_addr (fst addr)) (snd addr); *)
          H.remove peers addr
        end else begin
          p.retries <- p.retries + 1;
          p.timeout <- List.nth reconnect_wait p.retries;
          Lwt_log.ign_debug_f "Will retry to connect to %s:%d afer %.0fs"
            (Unix.string_of_inet_addr (fst addr)) (snd addr) p.timeout;
          Queue.push addr queue
        end

      method peer_disconnected id =
        let addr = Hashtbl.find wires id in
        H.remove connections addr;
        Hashtbl.remove wires id;
        mgr # remove addr;
        mgr # drain

      method handshake_ok addr (id : SHA1.t) =
        Hashtbl.add wires id addr;
        let p = H.find peers addr in
        p.reconnect <- true

      method handshake_failed addr =
        H.remove connections addr;
        mgr # remove addr;
        mgr # drain
    end

  class client info_hash =
    let id = SHA1.generate ~prefix:"OCAML" () in
    let t, u = Lwt.wait () in
    object (client)
      val peers = Hashtbl.create 0
      val peer_man = new peer_manager ()
      val mutable state = Incomplete (IncompleteMetadata.create (), u)
      val mutable optimistic_num = optimistic_unchoke_iterations;
      val mutable last_choke_unchoke = min_float
      val mutable listener = new dummy_listener

      method private request p pieces =
        if p # num_requests < max_requests then
          match Piece.request_active pieces (p # has) with
          | Some (i, j) ->
              let off = j * block_size in
              let len = min block_size (pieces.(i).Piece.length - off) in
              p # send_request i off len
          | None ->
              ()

      method private welcome sock exts id =
        let p = new Peer.peer ~id ~sock ~client in
        (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)
        p # send_extended_handshake;
        begin match state with
        | Complete {we_have; _} ->
            p # send_have_bitfield we_have
        | Incomplete _ ->
            ()
        end;
        p

      method connect addr =
        let push = function
          | Handshake.Ok (sock, ext, peer_id) ->
              Lwt_log.ign_notice_f "Connected to %s:%d [%a] successfully" (Unix.string_of_inet_addr (fst addr)) (snd addr)
                SHA1.sprint_hex_short peer_id;
              let p = client # welcome sock ext peer_id in
              listener # peer_joined peer_id (fst addr);
              Hashtbl.add peers peer_id p;
              peer_man # handshake_ok addr peer_id
          | Handshake.Failed ->
              Lwt_log.ign_error_f "Connection to %s:%d failed" (Unix.string_of_inet_addr (fst addr)) (snd addr);
              peer_man # handshake_failed addr
        in
        Handshake.outgoing ~id ~info_hash addr push

      method private update_choke_unchoke =
        let now = Unix.time () in
        if last_choke_unchoke +. rechoke_interval >= now then begin
          last_choke_unchoke <- now;
          optimistic_num <-
            if optimistic_num = 0 then optimistic_unchoke_iterations else optimistic_num - 1;
          let wires =
            let add _ p wires = (p, Random.int (1 lsl 29)) :: wires in
            Hashtbl.fold add peers []
          in
          let wires = List.sort rechoke_compare wires in
          let rec select n acc = function
            | (p, _) :: wires when n < rechoke_slots ->
                let n = if p # interested_in_us then n + 1 else n in
                select n (p :: acc) wires
            | wires ->
                let wires = List.filter (fun (p, _) -> p # interested_in_us) wires in
                if optimistic_num = 0 && List.length wires > 0 then
                  let p, _ = List.nth wires (Random.int (List.length wires)) in
                  (p :: acc), wires
                else
                  acc, wires
          in
          let to_unchoke, to_choke = select 0 [] wires in
          List.iter (fun p -> p # send_unchoke) to_unchoke;
          List.iter (fun (p, _) -> p # send_choke) to_choke
        end

      method private update_requests =
        match state with
        | Complete {pieces; _} ->
            Hashtbl.iter (fun _ p -> if not (p # choking_us) then client # request p pieces) peers
        | Incomplete _ ->
            ()

      method private update_interest =
        match state with
        | Complete {pieces; _} ->
            let rec loop i p =
              if i < Array.length pieces then
                if p # has i then
                  match pieces.(i).Piece.state with
                  | Piece.Active _
                  | Piece.Pending ->
                      p # send_interested
                  | Piece.Verified ->
                      loop (i + 1) p
                else
                  loop (i + 1) p
              else
                p # send_not_interested
            in
            Hashtbl.iter (fun _ p -> loop 0 p) peers
        | Incomplete _ ->
            ()

      method private send_keep_alives =
        let now = Unix.time () in
        let aux _ p = p # send_keep_alive in
        Hashtbl.iter aux peers

      method start =
        let rec malthusian_process () =
          client # update_interest;
          client # update_requests;
          client # update_choke_unchoke;
          client # send_keep_alives;
          Lwt_unix.sleep 1.0 >>= malthusian_process
        in
        Lwt.async malthusian_process

      method got_request p idx off len =
        match state with
        | Complete {pieces; store; _} ->
            if 0 <= idx && idx < Array.length pieces then
              begin match pieces.(idx).Piece.state with
              | Piece.Verified ->
                  let off1 = Int64.(add pieces.(idx).Piece.offset (of_int off)) in
                  Lwt.async (fun () ->
                    Store.read store off1 len >|= fun buf ->
                    p # send_piece idx off buf
                  )
              | Piece.Pending
              | Piece.Active _ ->
                  ()
              end
            else
              Lwt_log.ign_warning_f "PEER REQUEST invalid piece:%d" idx
        | Incomplete _ ->
            ()

      method got_metadata_size p len =
        match state with
        | Incomplete (m, _) ->
            if len <= IncompleteMetadata.metadata_max_size then begin
              try
                IncompleteMetadata.set_length m len;
                IncompleteMetadata.iter_missing (p # send_metadata_request) m;
              with _ ->
                IncompleteMetadata.reset m
            end else
              Lwt_log.ign_warning_f "METADATA length %d is too large, ignoring." len
        | Complete _ ->
            ()

      method got_choked p reqs =
        match state with
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

      method got_metadata_request p piece =
        match state with
        | Incomplete _ ->
            p # send_metadata_rejection piece
        | Complete {metadata; _} ->
            p # send_metadata_piece
              (Metadata.length metadata) piece
              (Metadata.get_piece metadata piece)

      method got_metadata_piece p piece data =
        match state with
        | Incomplete (m, gotit) ->
            if IncompleteMetadata.add m piece data then begin
              match IncompleteMetadata.verify m info_hash with
              | Some raw ->
                  Lwt.wakeup_later gotit raw
              (* debug "got full metadata"; *)
              (* let m = Metadata.create (Bcode.decode raw) in *)
              (* t.state <- Complete (m, *)
              (* Lwt.return m *)
              | None ->
                  IncompleteMetadata.reset m;
                  Lwt_log.ign_error "METADATA HASH CHECK FAILED"
            end
        | Complete _ ->
            ()

      method got_block p idx off s =
        match state with
        | Complete {we_have; store; pieces} ->
            if 0 <= idx && idx < Array.length pieces then begin
              let j = off / block_size in
              match pieces.(idx).Piece.state with
              | Piece.Pending ->
                  Lwt_log.ign_warning_f "Received block #%d for piece #%d, not requested ???" j idx
              | Piece.Verified ->
                  Lwt_log.ign_warning_f "Received block #%d for piece #%d, already completed" j idx
              | Piece.Active parts ->
                  let c = parts.(j) in
                  if c >= 0 then begin
                    parts.(j) <- (-1);
                    let do_cancel _ p =
                      if p # requested idx j (Cstruct.len s) then
                        p # send_cancel idx j (Cstruct.len s)
                    in
                    if c > 1 then Hashtbl.iter do_cancel peers;
                    pieces.(idx).Piece.have <- pieces.(idx).Piece.have + 1;
                    listener # block_received (p # id) idx off (Cstruct.len s);
                    Lwt_log.ign_notice_f "Received block #%d for piece #%d, have %d, missing %d"
                      j idx pieces.(idx).Piece.have (Array.length parts - pieces.(idx).Piece.have)
                  end;
                  let off = Int64.(add pieces.(idx).Piece.offset (of_int off)) in
                  Lwt.async (fun () ->
                    Store.write store off s >>= fun () ->
                    verify_piece store peers pieces.(idx) >>= function
                    | true ->
                        pieces.(idx).Piece.state <- Piece.Verified;
                        Bits.set we_have idx;
                        listener # piece_verified idx;
                        Hashtbl.iter (fun _ p -> p # send_have idx) peers;
                        Lwt.return_unit
                    (* maybe update interest ? *)
                    | false -> (* FIXME FIXME *)
                        Lwt.return_unit
                  )
            end
        | Incomplete _ ->
            ()

      method got_pex added =
        List.iter (fun (addr, _) -> peer_man # add addr) added

      method peer_disconnected p =
        peer_man # peer_disconnected (p # id);
        Hashtbl.remove peers (p # id)

      method set_listener lstnr =
        listener <- lstnr

      initializer
        peer_man # set_client (client :> connecter)
    end
end

class client info_hash =
  let cl = new Client.client info_hash in
  object (client)
    inherit Client.dummy_listener

    method start =
      cl # start

    initializer
      cl # set_listener (client :> Client.listener)
  end

(* open Lwt.Infix *)

(* type addr = Unix.inet_addr * int *)

(* let pex_delay = 60.0 *)

(* let rec pex_pulse pm = *)
(*   let pex = Hashtbl.fold (fun addr _ l -> addr :: l) pm.peers [] in *)
(*   iter_peers (fun p -> Peer.send_pex p pex) pm; *)
(*   Lwt_unix.sleep pex_delay >>= fun () -> pex_pulse pm *)

(* let start_server ?(port = 0) push = *)
(*   let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in *)
(*   Lwt_unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port)); *)
(*   let port = match Lwt_unix.getsockname fd with Unix.ADDR_INET (_, p) -> p | Unix.ADDR_UNIX _ -> assert false in *)
(*   Lwt_unix.listen fd listen_backlog; *)
(*   Log.debug "listening on port %u" port; *)
(*   let rec loop () = *)
(*     Lwt_unix.accept fd >>= fun (fd, sa) -> *)
(*     Log.debug "accepted connection from %s" (Util.string_of_sockaddr sa); *)
(*     let addr = match sa with Unix.ADDR_INET (ip, p) -> ip, p | Unix.ADDR_UNIX _ -> assert false in *)
(*     push addr (new Util.tcp fd); *)
(*     loop () *)
(*   in *)
(*   loop () *)

(* let start_server ?port () = *)
(*   let push addr sock = *)
(*     (\* FIXME *\) *)
(*     () *)
(*   in *)
(*   Lwt.ignore_result (start_server ?port push) *)

(* let start_trackers {Peer.id; info_hash; peer_man; _} trackers = *)
(*   let rec aux tracker = *)
(*     Tracker.announce ~info_hash id tracker >>= fun (interval, addrs) -> *)
(*     List.iter (PeerMgr.add peer_man) addrs; *)
(*     Lwt_unix.sleep interval >>= fun () -> *)
(*     aux tracker *)
(*   in *)
(*   Lwt.ignore_result (Lwt_list.iter_p aux trackers) *)

(* let start t trackers = *)
(*   start_server (); *)
(*   start_trackers t trackers *)

(* let create mg = *)
(*   (\* let ch, push = let ch, push = Lwt_stream.create () in ch, (fun x -> push (Some x)) in *\) *)
(*   let id = SHA1.generate ~prefix:"OCAML" () in *)
(*   let info_hash = mg.Magnet.xt in *)
(*   let sw = Peer.create id info_hash in *)
(*   start sw mg.Magnet.tr; *)
(*   sw *)
