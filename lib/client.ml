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

module Log  = Log.Make (struct let section = "[BT]" end)
module ARC4 = Nocrypto.Cipher_stream.ARC4
module Cs   = Nocrypto.Uncommon.Cs

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

module IncompleteMetadata = struct
  type t =
    {
      length : int;
      pieces : Bits.t;
      raw : Cstruct.t;
    }

  let metadata_block_size = 1 lsl 14
  let metadata_max_size = 1 lsl 22

  let create ~length =
    let size = (length + metadata_block_size - 1) / metadata_block_size in
    { length; pieces = Bits.create size; raw = Cstruct.create length }

  let add m n buf =
    if n < 0 || n >= Bits.length m.pieces then invalid_arg "add";
    Bits.set m.pieces n;
    Cstruct.blit buf 0 m.raw (n * metadata_block_size) (Cstruct.len buf);
    Bits.has_all m.pieces

  let verify m info_hash =
    if not (Bits.has_all m.pieces) then invalid_arg "IncompleteMetadata.verify";
    if SHA1.(equal (digest m.raw) info_hash) then
      Some m.raw
    else
      None

  let iter_missing f m =
    for i = 0 to Bits.length m.pieces - 1 do
      if not (Bits.is_set m.pieces i) then
        f i
    done
end

module Handshake = struct

  module Encryption = struct

    module Dh   = Nocrypto.Dh
    module Z    = Nocrypto.Numeric.Z
    module SHA1 = Nocrypto.Hash.SHA1

    let () =
      Random.self_init ();
      Nocrypto.Rng.reseed (Cstruct.of_string (string_of_int (Random.bits ())))

    (* let crypto_plain = 0x01l *)
    (* let crypto_rc4 = 0x02l *)
    (* let crypto_provide = crypto_rc4 *)
    let max_pad_len = 512
    (* let key_len = 96 *)

(*
1 A->B: Diffie Hellman Ya, PadA
2 B->A: Diffie Hellman Yb, PadB
3 A->B: HASH('req1', S), HASH('req2', SKEY) xor HASH('req3', S), ENCRYPT(VC, crypto_provide, len(PadC), PadC, len(IA)), ENCRYPT(IA)
4 B->A: ENCRYPT(VC, crypto_select, len(padD), padD), ENCRYPT2(Payload Stream)
5 A->B: ENCRYPT2(Payload Stream)
*)

(*
   handshake: <pstrlen><pstr><reserved><info_hash><peer_id>
   pstrlen: string length of <pstr>, as a single raw byte
   pstr: string identifier of the protocol
   reserved: eight (8) reserved bytes.
   info_hash: 20-byte SHA1 hash of the info key in the metainfo file.
   peer_id: 20-byte string used as a unique ID for the client.
*)

    let dh_prime =
      "FF FF FF FF FF FF FF FF C9 0F DA A2
     21 68 C2 34 C4 C6 62 8B 80 DC 1C D1
     29 02 4E 08 8A 67 CC 74 02 0B BE A6
     3B 13 9B 22 51 4A 08 79 8E 34 04 DD
     EF 95 19 B3 CD 3A 43 1B 30 2B 0A 6D
     F2 5F 14 37 4F E1 35 6D 6D 51 C2 45
     E4 85 B5 76 62 5E 7E C6 F4 4C 42 E9
     A6 3A 36 21 00 00 00 00 00 09 05 63"

    let g =
      { Dh.p = Z.of_cstruct_be @@ Cs.of_hex dh_prime;
        gg = Z.of_int 2;
        q = None }

    let priv, pub = Dh.gen_key g

    type encryption_mode =
      | Encrypted
      | Both
      | Plain

    type server_state =
      | ServerWaitDh

    type client_state =
      | ClientWaitDh
      | ClientSyncVC of ARC4.key * ARC4.key * int
      | ClientWaitCryptoSelect of ARC4.key * ARC4.key
      | ClientWaitPadD of ARC4.key * ARC4.key * int32 * int

    type state =
      | Client of client_state
      | Server of server_state

    type t = {
      info_hash : Cstruct.t;
      mode : encryption_mode;
      buf : Cstruct.t;
      state : state
    }

    let select mode crypto_provide =
      match mode, crypto_provide with
      | Plain, 0x01l
      | Plain, 0x03l -> Some 0x01l
      | Encrypted, 0x2l
      | Encrypted, 0x3l
      | Both, 0x2l
      | Both, 0x3l -> Some 0x2l
      | Both, 0x1l -> Some 0x1l
      | _ -> None

    let provide = function
      | Plain -> 0x01l
      | Encrypted -> 0x02l
      | Both -> 0x03l

    let keyA = Cstruct.of_string "keyA"
    let keyB = Cstruct.of_string "keyB"
    let req1 = Cstruct.of_string "req1"
    let req2 = Cstruct.of_string "req2"
    let req3 = Cstruct.of_string "req3"
    let vc   = Cstruct.of_string "\x00\x00\x00\x00\x00\x00\x00\x00"

    let arc4 =
      let discard = Cstruct.create 1024 in
      fun from secret skey ->
        let key = ARC4.of_secret @@ SHA1.digestv [ from; secret; skey ] in
        let { ARC4.key } = ARC4.encrypt ~key discard in
        key

    let cs_int16 n =
      let cs = Cstruct.create 2 in
      Cstruct.BE.set_uint16 cs 0 n;
      cs

    let cs_int32 n =
      let cs = Cstruct.create 4 in
      Cstruct.BE.set_uint32 cs 0 n;
      cs

    let generate n =
      let cs = Cstruct.create n in
      for i = 0 to n - 1 do
        Cstruct.set_uint8 cs i (Random.int 256)
      done;
      cs

    let handle_client t state =
      let rec loop buf st =
        let len = Cstruct.len buf in
        match st with
        | ClientWaitDh when len >= Dh.modulus_size g ->
            let yb, buf = Cstruct.split buf (Dh.modulus_size g) in
            let shared_secret = Dh.shared g priv yb in
            let my_key = arc4 keyA shared_secret t.info_hash in
            let her_key = arc4 keyB shared_secret t.info_hash in
            let padC_len = Random.int (max_pad_len + 1) in
            let padC = generate padC_len in
            let { ARC4.key = my_key; message } =
              ARC4.encrypt ~key:my_key
                (Cs.concat [ vc; cs_int32 (provide t.mode); cs_int16 padC_len; padC; cs_int16 0 ])
            in
            let cs =
              Cs.concat
                [ SHA1.digestv [ req1; shared_secret ];
                  Cs.xor (SHA1.digestv [ req2; t.info_hash ]) (SHA1.digestv [ req3; shared_secret ]);
                  message ]
            in
            `Ok ({ t with buf; state = Client (ClientSyncVC (my_key, her_key, 0)) }, Some cs)

        | ClientWaitDh ->
            `Ok (t, None)

        | ClientSyncVC (_, _, off) when off >= max_pad_len ->
            `Error "couldn't find VC"

        | ClientSyncVC (_, _, off) when off + Cstruct.len vc > len ->
            `Ok (t, None)

        | ClientSyncVC (my_key, her_key, off) ->
            let { ARC4.key = her_key; message = vc' } =
              ARC4.decrypt ~key:her_key (Cstruct.sub buf off (Cstruct.len vc))
            in
            if Cs.equal vc' vc then
              let buf = Cstruct.shift buf (Cstruct.len vc) in
              loop buf (ClientWaitCryptoSelect (my_key, her_key))
            else
              loop buf (ClientSyncVC (my_key, her_key, off + 1))

        | ClientWaitCryptoSelect (my_key, her_key) when len >= 6 ->
            let message, buf = Cstruct.split buf 6 in
            let { ARC4.key = her_key; message } = ARC4.decrypt her_key message in
            let crypto_select = Cstruct.BE.get_uint32 message 0 in
            let padD_len = Cstruct.BE.get_uint16 message 4 in
            loop buf (ClientWaitPadD (my_key, her_key, crypto_select, padD_len))

        | ClientWaitCryptoSelect _ ->
            `Ok (t, None)

        | ClientWaitPadD (my_key, her_key, crypto_select, padD_len) when len >= padD_len ->
            let padD, buf = Cstruct.split buf padD_len in
            let { ARC4.key = her_key } = ARC4.decrypt ~key:her_key padD in
            let mode = match crypto_select with
              | 1l -> None
              | 2l -> Some (my_key, her_key)
              | _ -> failwith "bad crypto_select"
            in
            `Success (mode, buf)

        | ClientWaitPadD _ ->
            `Ok (t, None)

      in
      loop t.buf state

    let handle_server _ _ =
      assert false

    let (<+>) = Cs.(<+>)

    let handle t buf =
      let t = { t with buf = t.buf <+> buf } in
      match t.state with
      | Client state ->
          handle_client t state
      | Server state ->
          handle_server t state

    type ret =
      [ `Ok of t * Cstruct.t option
      | `Success of (ARC4.key * ARC4.key) option * Cstruct.t
      | `Error of string ]

    let outgoing ~info_hash mode =
      let t = { info_hash; mode; state = Client ClientWaitDh; buf = Cs.empty } in
      let padA_len = Random.int (max_pad_len + 1) in
      let padA = generate padA_len in
      `Ok (t, Some (pub <+> padA))

    let incoming ~info_hash mode =
      `Error "not implemented"

    type result =
      | Ok of Util.socket * Cstruct.t
      | Failed

    open Lwt.Infix

    let buf_size = 1024

    let negotiate sock t =
      let read_buf = Cstruct.create buf_size in
      let rec loop = function
        | `Ok (t, Some cs) ->
            Lwt_cstruct.complete (sock # write) cs >>= fun () ->
            loop (handle t Cs.empty)
        | `Ok (t, None) ->
            sock # read read_buf >>= begin function
            | 0 ->
                Lwt.fail End_of_file
            | n ->
                loop (handle t (Cstruct.sub read_buf 0 n))
            end
        | `Error err ->
            Lwt.fail (Failure err)
        | `Success (None, rest) ->
            Lwt.return (sock, rest)
        | `Success (Some (enc, dec), rest) ->
            Lwt.return (new Util.encrypt sock enc dec, rest)
      in
      loop t

    let outgoing ~info_hash sock mode =
      negotiate sock (outgoing ~info_hash mode)

  end

  (* IO *)

  open Lwt.Infix

  let proto = Cstruct.of_string "\019BitTorrent protocol"

  let ltep_bit = 43 (* 20-th bit from the right *)
  let dht_bit = 63 (* last bit of the extension bitfield *)

  let extensions =
    let bits = Bits.create (8 * 8) in
    Bits.set bits ltep_bit;
    (* Bits.set bits Wire.dht_bit; *)
    Bits.to_cstruct bits

  let handshake_len =
    Cstruct.len proto + 8 (* extensions *) + 20 (* info_hash *) + 20 (* peer_id *)

  let handshake_message ~id ~info_hash =
    Cs.concat [ proto; extensions; SHA1.to_raw info_hash; SHA1.to_raw id ]

  let parse_handshake cs =
    if Cstruct.len cs != handshake_len then invalid_arg "parse_handshake";
    match Cstruct.get_uint8 cs 0 with
    | 19 ->
        let proto' = Cstruct.sub cs 0 20 in
        if Cs.equal proto' proto then
          let ext = Bits.of_cstruct @@ Cstruct.sub cs 20 8 in
          let info_hash = Cstruct.sub cs 28 20 in
          let peer_id = Cstruct.sub cs 48 20 in
          `Ok (ext, SHA1.of_raw info_hash, SHA1.of_raw peer_id)
        else
          `Error (Printf.sprintf "unknown protocol: %S" (Cstruct.to_string proto'))
    | n ->
        `Error (Printf.sprintf "bad protocol length %d" n)

  let outgoing_handshake ~id ~info_hash sock rest =
    Lwt_cstruct.complete (sock # write) (handshake_message id info_hash) >>= fun () ->
    assert (Cstruct.len rest <= handshake_len);
    let hs = Cstruct.create handshake_len in
    Cstruct.blit rest 0 hs 0 (Cstruct.len rest);
    Lwt_cstruct.complete (sock # read) (Cstruct.shift hs (Cstruct.len rest)) >>= fun () ->
    match parse_handshake hs with
    | `Ok (ext, info_hash', peer_id) ->
        if SHA1.equal info_hash info_hash' then begin
          Lwt.return (sock, ext, peer_id)
        end else
          Lwt.fail (Failure "bad info-hash")
    | `Error err ->
        Lwt.fail (Failure err)

  type kind =
    | Plain
    | Encrypted

  type addr = Unix.inet_addr * int

  (* type result = *)
  (*   | Ok of Util.socket * Bits.t * SHA1.t *)
  (*   | Failed *)

  let with_socket f =
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    try%lwt
      f fd
    with e ->
      let%lwt () = try%lwt Lwt_unix.close fd with _ -> Lwt.return_unit in
      [%lwt raise e]

  let connect kind ~id ~info_hash addr fd =
    let ip, port = addr in
    Log.info "Connecting [%s] to %s:%d..."
      (match kind with Encrypted -> "ENCRYPTED" | Plain -> "PLAIN")
      (Unix.string_of_inet_addr ip) port;
    let sa = Lwt_unix.ADDR_INET (ip, port) in
    let%lwt () = Lwt_unix.connect fd sa in
    let%lwt (sock, rest) =
      let sock = new Util.tcp fd in
      match kind with
      | Encrypted ->
          Encryption.outgoing ~info_hash:(SHA1.to_raw info_hash) sock Encryption.Both
      | Plain ->
          Lwt.return (sock, Cs.empty)
    in
    outgoing_handshake ~id ~info_hash sock rest

  let connect ~id ~info_hash addr =
    with_socket (fun fd -> connect Plain ~id ~info_hash addr fd)
    (* | sock, ext, peer_id -> *)
    (*   (fun (sock, ext, peer_id) -> *)
    (*      Lwt.wrap1 push @@ Ok (sock, ext, peer_id)) *)
    (*   (function *)
    (*     | e -> *)
    (*         Log.error "%s" (Printexc.to_string e); *)
    (*         Lwt.wrap1 push Failed) |> Lwt.ignore_result *)
    (* | Unix.Unix_error (_, "connect", _) -> *)
    (*     Lwt.wrap1 push Failed *)
    (* | _ -> *)
    (*     Log.error "Encrypted connection to %s:%d failed, retrying with plain..." *)
    (*       (Unix.string_of_inet_addr (fst addr)) (snd addr); *)
    (*     Lwt.try_bind (fun () -> outgoing Plain ~id ~info_hash addr) *)
    (*       (fun (sock, ext, peer_id) -> Lwt.wrap1 push @@ Ok (sock, ext, peer_id)) *)
    (*       (fun _ -> Lwt.wrap1 push Failed)) |> Lwt.ignore_result *)
end

module Peers = struct
  type addr = Unix.inet_addr * int

  type pex_flags =
    { pex_encryption : bool;
      pex_seed : bool;
      pex_utp : bool;
      pex_holepunch : bool;
      pex_outgoing : bool }

  let keepalive_delay = 20. (* FIXME *)

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
    | GotPEX of (addr * pex_flags) list * addr list
    | DHTPort of int
    | HandshakeFailed of addr
    | HandshakeOk of addr * Util.socket * Bits.t * SHA1.t

  type ut_extension =
    | UT_pex
    | UT_metadata

  let string_of_ut_extension = function
    | UT_pex -> "ut_pex"
    | UT_metadata -> "ut_metadata"

  let ut_extension_of_string = function
    | "ut_pex" -> UT_pex
    | "ut_metadata" -> UT_metadata
    | s -> Printf.ksprintf failwith "unknown UT extension %S" s

  type peer =
    {
      id                      : SHA1.t;
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
      upload                  : Speedometer.t;
      download                : Speedometer.t;
      requests                : (int * int * int) Lwt_sequence.t;
      peer_requests           : (int * int * int) Lwt_sequence.t;
      send                    : unit Lwt_condition.t;
      queue                   : Wire.message Lwt_sequence.t;
    }

  type piece_state =
    | Pending
    | Active of int array
    | Verified

  type piece =
    {
      mutable state : piece_state;
      length : int;
      offset : int64;
      mutable have : int;
      hash : SHA1.t;
    }

  type metadata =
    | Complete of Metadata.t
    | Incomplete of IncompleteMetadata.t

  type t =
    {
      id : SHA1.t;
      info_hash : SHA1.t;
      peers : (SHA1.t, peer) Hashtbl.t;
      mutable pieces : piece array;
      mutable metadata : metadata;
      mutable has_metadata : bool;
    }

  let count t =
    Hashtbl.length t.peers

  let connect t ~id ~info_hash addr =
    (* let push = function *)
    (*   | Handshake.Ok (sock, ext, peer_id) -> *)
    (*      Log.info "Connected to %s:%d [%a] successfully" (Unix.string_of_inet_addr (fst addr)) (snd addr) *)
    (*        SHA1.print_hex_short peer_id; *)
    (*      HandshakeOk (addr, sock, ext, peer_id) |> push *)
    (*   | Handshake.Failed -> *)
    (*      Log.error "Connection to %s:%d failed" (Unix.string_of_inet_addr (fst addr)) (snd addr); *)
    (*      HandshakeFailed addr |> push *)
    (* in *)
    match%lwt Handshake.connect ~id ~info_hash addr with
    | exception _ ->
        [%lwt failwith "connect failed"]
    (* Lwt.wrap2 t.push id (HandshakeFailed addr) (\* FIXME FIXME peer_id NOT id *\) *)
    | sock, ext, peer_id ->
        Lwt.return_unit
  (* Lwt.wrapt.push peer_id (HandshakeOk (addr, sock, ext, peer_id)) *)

  (* let welcome sock exts id = *)
  (*   create_peer id t.push sock *)
  (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)

  (* let peer_choking p = *)
  (*   p.peer_choking *)

  (* let peer_interested p = *)
  (*   p.peer_interested *)

  (* let has p i = *)
  (*   if i < 0 then invalid_arg "Peer.has"; *)
  (*   i < Bits.length p.have && Bits.is_set p.have i *)

  (* let am_choking p = *)
  (*   p.am_choking *)

  (* let am_interested p = *)
  (*   p.am_interested *)

  let download_speed p =
    Speedometer.speed p.download

  let upload_speed p =
    Speedometer.speed p.upload

  (* let requests p = *)
  (*   Lwt_sequence.length p.requests *)

  (* let requested p i off len = *)
  (*   match Lwt_sequence.find_node_opt_l (fun (i', off', len') -> i = i' && off = off' && len = len') p.requests with *)
  (*   | None -> *)
  (*       false *)
  (*   | Some _ -> *)
  (*       true *)

  (* let worked_on_piece p i = *)
  (*   if i < 0 then invalid_arg "Peer.worked_on_piece"; *)
  (*   i < Bits.length p.blame && Bits.is_set p.blame i *)

  (* let strike p = *)
  (*   p.strikes <- p.strikes + 1; *)
  (*   p.strikes *)

  let got_ut_metadata t p data =
    let m, data = Bcode.decode_partial data in
    let msg_type = Bcode.to_int (Bcode.find "msg_type" m) in
    let piece = Bcode.to_int (Bcode.find "piece" m) in
    match t.metadata, msg_type with
    | Incomplete _, 0 -> (* request *)
        let id = Hashtbl.find p.extensions UT_metadata in
        let m =
          let d = [ "msg_type", Bcode.Int 2L; "piece", Bcode.Int (Int64.of_int piece) ] in
          Bcode.encode (Bcode.Dict d)
        in
        send p @@ Wire.EXTENDED (id, m)
    | Complete m, 0 ->
        let metadata_piece len i data p =
          let id = Hashtbl.find p.extensions UT_metadata in
          let m =
            let d =
              [ "msg_type",   Bcode.Int 1L;
                "piece",      Bcode.Int (Int64.of_int piece);
                "total_size", Bcode.Int (Int64.of_int len) ]
            in
            Cs.(Bcode.encode (Bcode.Dict d) <+> data)
          in
          send p @@ Wire.EXTENDED (id, m)
        in
        metadata_piece (Metadata.length meta) piece (Metadata.get_piece meta piece)
    | Complete _, 1 -> (* data *)
        ()
    | Incomplete m, 1 ->
        if IncompleteMetadata.add m' i s then begin
          match IncompleteMetadata.verify m' bt.ih with
          | Some raw ->
              (* debug "got full metadata"; *)
              let m' = Metadata.create (Bcode.decode raw) in
              Lwt.return (m', peers)
          | None ->
              Log.error "METADATA HASH CHECK FAILED";
              loop None
        end
    | _ ->
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
    t.push p.id (GotPEX (List.combine added added_f, loop dropped))

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
    (* TODO emit Uploaded event *)
    send p @@ Wire.PIECE (i, o, buf)

  let request p i ofs len =
    if p.peer_choking then invalid_arg "Peer.request";
    ignore (Lwt_sequence.add_r (i, ofs, len) p.requests);
    send p @@ Wire.REQUEST (i, ofs, len)

  let extended_handshake p =
    let m =
      List.map (fun (id, (name, _)) ->
          name, Bcode.Int (Int64.of_int id)) supported_extensions
    in
    let m = Bcode.Dict ["m", Bcode.Dict m] in
    Log.info "> EXTENDED HANDSHAKE id:%a (%s)" SHA1.print_hex_short p.id
      (String.concat " " (List.map (fun (n, (name, _)) -> Printf.sprintf "%s %d" name n) supported_extensions));
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

  let listen_backlog = 5
  let listen_ports = [50000]
  let choke_timeout = 5.
  let rechoke_interval = 10.
  let rechoke_optimistic_duration = 2
  let rechoke_slots = 10
  let block_size = 1 lsl 14 (* 16384 *)
  let max_requests = 5

  let rechoke t opt nopt =
    (* Log.info "RECHOKING"; *)
    let opt, nopt = if nopt > 0 then opt, nopt - 1 else None, nopt in
    let wires =
      let add _ p wires =
        match (* Peer.is_seeder p, *)false, opt with (* FIXME FIXME *)
        | true, _ ->
            choke p;
            wires
        | false, Some opt when SHA1.equal p.id opt ->
            wires
        | false, _ ->
            (p, Random.int (1 lsl 29)) :: wires
      in
      Hashtbl.fold add t.peers []
    in
    let wires = List.sort rechoke_compare wires in
    (* Log.debug "RECHOKE %d TOTAL" (List.length wires); *)
    let rec select n acc = function
      | (p, _) as w :: wires when n < rechoke_slots ->
          let n = if p.peer_interested then n + 1 else n in
          select n (w :: acc) wires
      | wires ->
          begin match opt with
          | Some _ ->
              acc, wires, opt, nopt
          | None ->
              let wires = List.filter (fun (p, _) -> p.peer_interested) wires in
              if List.length wires > 0 then
                let (p, _) as opt = List.nth wires (Random.int (List.length wires)) in
                (opt :: acc), wires, Some p.id, rechoke_optimistic_duration
              else
                acc, wires, None, nopt
          end
    in
    let to_unchoke, to_choke, opt, nopt = select 0 [] wires in
    (* Log.debug "RECHOKE total=%d unchoke=%d choke=%d" (Hashtbl.length peers) (List.length unchoke) *)
    (*   (List.length choke); *)
    List.iter (fun (p, _) -> unchoke p) to_unchoke;
    List.iter (fun (p, _) -> choke p) to_choke;
    (opt, nopt)

  let have t i =
    let aux _ p = send p @@ Wire.HAVE i in
    Hashtbl.iter aux t.peers

  let have_bitfield t bits =
    let aux id p =
      (* check for redefined length FIXME *)
      Bits.set_length p.have (Bits.length bits);
      Bits.set_length p.blame (Bits.length bits);
      Log.info "> BITFIELD id:%a have:%d total:%d" SHA1.print_hex_short p.id
        (Bits.count_ones bits) (Bits.length bits);
      send p @@ Wire.BITFIELD bits
    in
    Hashtbl.iter aux t.peers

  let cancel p i o l =
    (* Log.info "> CANCEL id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i o l; *)
    send p @@ Wire.CANCEL (i, o, l);
    try
      let n = Lwt_sequence.find_node_l (fun (i', o', l') -> i = i' && o' = o && l = l') p.requests in
      Lwt_sequence.remove n
    with
    | Not_found ->
        Log.warn "! REQUEST NOT FOUND id:%a idx:%d off:%d len:%d" SHA1.print_hex_short p.id i o l

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
    Log.info "> PEX id:%a added:%d dropped:%d" SHA1.print_hex_short p.id
      (List.length added) (List.length dropped);
    send p @@ Wire.EXTENDED (id, Bcode.encode @@ Bcode.Dict d)

  let send_pex pex p =
    if Hashtbl.mem p.extensions UT_pex then begin
      let added = List.filter (fun a -> not (List.mem a p.last_pex)) pex in
      let dropped = List.filter (fun a -> not (List.mem a pex)) p.last_pex in
      p.last_pex <- pex;
      send_ut_pex p added dropped
    end

  let on_message t p m =
    match m with
    | Wire.KEEP_ALIVE ->
        ()
    | Wire.CHOKE ->
        if not p.peer_choking then begin
          p.peer_choking <- true;
          let reqs = Lwt_sequence.fold_l (fun r l -> r :: l) p.requests [] in
          Lwt_sequence.iter_node_l (fun n -> Lwt_sequence.remove n) p.requests;
          t.push p.id (Choked reqs)
        end
    | Wire.UNCHOKE ->
        if p.peer_choking then begin
          p.peer_choking <- false;
          update_requests t
        end
    | Wire.INTERESTED ->
        if not p.peer_interested then begin
          p.peer_interested <- true;
          t.push p.id Interested
        end
    | Wire.NOT_INTERESTED ->
        if p.peer_interested then begin
          p.peer_interested <- false;
          t.push p.id NotInterested
        end
    | Wire.HAVE i ->
        (* check for got_bitfield_already FIXME *)
        Bits.resize p.have (i + 1);
        if not (Bits.is_set p.have i) then begin
          Bits.set p.have i;
          t.push p.id (Have i)
        end
    | Wire.BITFIELD b ->
        (* check for redefinition *)
        Bits.set_length p.have (Bits.length b);
        Bits.blit b 0 p.have 0 (Bits.length b);
        (* Log.info "< BITFIELD id:%s have:%d total:%d" (SHA1.to_hex_short p.id) (Bits.count_ones b) (Bits.length b); *)
        t.push p.id (HaveBitfield p.have)
    | Wire.REQUEST (i, off, len) ->
        if not p.am_choking then begin
          let (_ : _ Lwt_sequence.node) = Lwt_sequence.add_r (i, off, len) p.peer_requests in
          (* Log.info "< REQUEST id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i off len; *)
          t.push p.id (BlockRequested (i, off, len))
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
        t.push p.id (BlockReceived (i, off, s))
    | Wire.CANCEL (i, off, len) ->
        (* Log.info "< CANCEL id:%s idx:%d off:%d len:%d" (SHA1.to_hex_short p.id) i off len; *)
        let n =
          Lwt_sequence.find_node_opt_l
            (fun (i', off', len') -> i = i' && off = off' && len = len')
            p.peer_requests
        in
        begin match n with
        | Some n ->
            (* FIXME broadcast event *)
            Lwt_sequence.remove n
        | None ->
            Log.warn "! PEER REQUEST NOT FOUND id:%a idx:%d off:%d len:%d"
              SHA1.print_hex_short p.id i off len
        end
    | Wire.EXTENDED (0, s) ->
        let bc = Bcode.decode s in
        let m =
          Bcode.find "m" bc |> Bcode.to_dict |>
          List.map (fun (name, id) -> (ut_extension_of_string name, Bcode.to_int id))
        in
        List.iter (fun (name, id) ->
            if id = 0 then Hashtbl.remove p.extensions name
            else Hashtbl.replace p.extensions name id) m;
        Log.info "< EXTENDED HANDSHAKE id:%a (%s)" SHA1.print_hex_short p.id
          (String.concat " " (List.map (fun (name, n) -> Printf.sprintf "%s %d" (string_of_ut_extension name) n) m));
        if Hashtbl.mem p.extensions UT_metadata then
          t.push p.id (AvailableMetadata (Bcode.find "metadata_size" bc |> Bcode.to_int))
    | Wire.EXTENDED (id, data) ->
        Log.info "< EXTENDED id:%a mid:%d" SHA1.print_hex_short p.id id;
        let (_, f) = List.assoc id supported_extensions in
        f t p data
    | Wire.PORT i ->
        t.push p.id (DHTPort i)
    | _ ->
        ()

  (* Event loop *)

  let handle_err t p sock e =
    Log.error "ERROR id:%a exn:%S" SHA1.print_hex_short p.id (Printexc.to_string e);
    let reqs = Lwt_sequence.fold_l (fun r l -> r :: l) p.requests [] in
    t.push p.id (PeerDisconnected reqs);
    try%lwt sock # close with _ -> Lwt.return_unit

  let reader_loop t p sock =
    let buf = Cstruct.create Wire.max_packet_len in
    let rec loop off =
      match%lwt Lwt_unix.with_timeout keepalive_delay (fun () -> sock # read (Cstruct.shift buf off)) with
      | 0 ->
          [%lwt raise End_of_file]
      | n ->
          let n = n + off in
          let msgs, off = Wire.handle (Cstruct.sub buf 0 n) in
          Cstruct.blit buf off buf 0 (n - off);
          List.iter
            (fun m ->
               Log.debug "[%a] --> %a" SHA1.print_hex_short p.id Wire.print m) msgs;
          List.iter (on_message t p) msgs;
          loop (n - off)
    in
    loop 0

  let writer_loop t p sock =
    let buf = Cstruct.create Wire.max_packet_len in
    let write m =
      Log.debug "[%a] <-- %a" SHA1.print_hex_short p.id Wire.print m;
      let buf = Util.W.into_cstruct (Wire.writer m) buf in
      Lwt_cstruct.complete (sock # write) buf
    in
    let rec loop () =
      match%lwt Lwt.pick
        [ (let%lwt () = Lwt_condition.wait p.send in Lwt.return `Ok);
          (let%lwt () = Lwt_unix.sleep keepalive_delay in Lwt.return `Timeout) ]
      with
      | `Ok ->
          let rec loop' () =
            match Lwt_sequence.take_opt_l p.queue with
            | Some m ->
                let%lwt () = write m in
                loop' ()
            | None ->
                Lwt.return_unit
          in
          let%lwt () = loop' () in
          loop ()
      | `Timeout ->
          let%lwt () = write Wire.KEEP_ALIVE in
          loop ()
    in
    loop ()

  let start t p sock =
    try%lwt
      Lwt.pick [reader_loop t p sock; writer_loop t p sock]
    with exn ->
      handle_err t p sock exn

  let create_peer t id push sock =
    let p =
      { id;
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
      }
    in
    Lwt.ignore_result (start t p sock);
    extended_handshake p;
    p

  let create push =
    {
      peers = Hashtbl.create 0;
      push;
    }
end

let listen_backlog = 5
let listen_ports = [50000]
let choke_timeout = 5.
let rechoke_interval = 10.
let rechoke_optimistic_duration = 2
let rechoke_slots = 10
let block_size = 1 lsl 14 (* 16384 *)
let max_requests = 5

type addr = Unix.inet_addr * int

(* type event = *)
(*   | PeersReceived of addr list *)
(*   | ConnectToPeer of addr * float *)
(*   | IncomingPeer of addr * Util.socket *)
(*   | PieceVerified of int *)
(*   | PieceFailed of int *)
(*   | TorrentComplete *)
(*   | PeerEvent of SHA1.t * Peer.event *)
(*   | BlockReadyToSend of SHA1.t * int * int * Cstruct.t *)
(*   | Error of exn *)
(*   | Rechoke of SHA1.t option * int *)

type t =
  {
    id : SHA1.t;
    ih : SHA1.t;
    trackers : Uri.t list;
    peer_mgr : PeerMgr.swarm;
  }

(* let pex_delay = 60.0 *)

(* let rec pex_pulse pm = *)
(*   let pex = Hashtbl.fold (fun addr _ l -> addr :: l) pm.peers [] in *)
(*   iter_peers (fun p -> Peer.send_pex p pex) pm; *)
(*   Lwt_unix.sleep pex_delay >>= fun () -> pex_pulse pm *)

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
  | None -> request_pending pieces has 0 n
  | Some _ as r -> r

let request_active pieces has =
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
    let res =
      match request_active pieces (Peer.has p) with
      | Some _ as res -> res
      | None ->
          match request_pending pieces has with
          | Some _ as res -> res
          | None -> request_endgame pieces has
    in
    match res with
    | Some (i, j) ->
        let off = j * block_size in
        let len = min block_size (pieces.(i).length - off) in
        Peers.send_request p i off len
    | None ->
        ()

let update_requests t =
  if t.has_metadata then begin
    let request _ p = if not p.peer_choking then request p t.pieces in
    Hashtbl.iter request t.peers
  end

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

let upon t f g =
  Lwt.async (fun () -> Lwt.try_bind t (Lwt.wrap1 f) (Lwt.wrap1 g))

let send_block store pieces p i off len push =
  match pieces.(i).state with
  | Verified ->
      let off1 = Int64.(add pieces.(i).offset (of_int off)) in
      upon (fun () -> Store.read store off1 len)
        (fun buf -> push (BlockReadyToSend (p, i, off, buf)))
        (fun e -> push (Error e))
  | Pending
  | Active _ ->
      ()

let verify_piece store peers pieces i push =
  upon (fun () -> Store.digest store pieces.(i).offset pieces.(i).length)
    (fun sha ->
       push (if SHA1.equal sha pieces.(i).hash then PieceVerified i else PieceFailed i)
    )
    (fun e -> push (Error e))

let record_block store peers pieces i off s push =
  let j = off / block_size in
  match pieces.(i).state with
  | Pending ->
      Log.warn "Received block #%d for piece #%d, not requested ???" j i
  | Verified ->
      Log.warn "Received block #%d for piece #%d, already completed" j i
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
      upon (fun () -> Store.write store off s)
        (fun () ->
           if pieces.(i).have = Array.length parts then
             verify_piece store peers pieces i push
        )
        (fun e -> push (Error e))

let request_rejected pieces (i, off, _) =
  match pieces.(i).state with
  | Verified
  | Pending -> ()
  | Active parts ->
      let j = off / block_size in
      if parts.(j) > 0 then parts.(j) <- parts.(j) - 1

let share_torrent bt meta store pieces have (peers : Peers.t) =
  Log.info "TORRENT SHARE have:%d total:%d peers:%d"
    (Bits.count_ones have) (Bits.length have) (Peers.count peers);
  (* let peer id f = try let p = Hashtbl.find peers id in f p with Not_found -> () in *)
  Peers.have_bitfield peers have;
  (* Hashtbl.iter (fun _ p -> Peer.have_bitfield p have; update_interest pieces p) peers; *)
  update_requests t;
  (* bt.push (Rechoke (None, rechoke_optimistic_duration)); *)
  let handle = function
    (* log_event e; *)
    (* | Rechoke (opt, nopt) -> *)
    (*     Peer.rechoke peers *)
    (* let opt, nopt = rechoke peers opt nopt in *)
    (* upon (fun () -> Lwt_unix.sleep choke_timeout) *)
    (*   (fun () -> bt.push (Rechoke (opt, nopt))) *)
    (*   (fun e -> bt.push (Error e)) *)

    | TorrentComplete ->
        (* FIXME TODO FIXME *)
        Log.info "TORRENT COMPLETE";
        raise Exit

    | PeersReceived addrs ->
        List.iter (PeerMgr.add bt.peer_mgr) addrs

    | PieceVerified i ->
        pieces.(i).state <- Verified;
        Bits.set have i;
        Peer.have peers i
    (* maybe update interest ? *)

    | PieceFailed i ->
        (* FIXME FIXME *)
        ()

    | Error e ->
        raise e

    | HandshakeFailed addr ->
        PeerMgr.handshake_failed bt.peer_mgr addr

    | PeerEvent (id, Peer.PeerDisconnected reqs) ->
        List.iter (request_rejected pieces) reqs;
        PeerMgr.peer_disconnected bt.peer_mgr id;
        Hashtbl.remove peers id
    (* if not (am_choking peers id) && peer_interested peers id then Choker.rechoke ch; *)

    | PeerEvent (_, Peer.Choked reqs) ->
        List.iter (request_rejected pieces) reqs

    | PeerEvent (id, Peer.Unchoked)
    | PeerEvent (id, Peer.Have _)
    | PeerEvent (id, Peer.HaveBitfield _) ->
        peer id (update_interest pieces);
        update_requests pieces peers

    | PeerEvent (_, Peer.Interested)
    | PeerEvent (_, Peer.NotInterested) ->
        (* rechoke *)
        ()

    | PeerEvent (id, Peer.MetaRequested i) ->
        peer id
          (Peer.metadata_piece (Metadata.length meta) i (Metadata.get_piece meta i))

    | PeerEvent (id, Peer.BlockRequested (i, off, len)) ->
        if i >= 0 && i < Array.length pieces then
          send_block store pieces id i off len bt.push
        else
          Log.warn "! PEER REQUEST invalid piece:%d" i

    | BlockReadyToSend (id, idx, ofs, buf) ->
        begin match Hashtbl.find peers id with
        | exception Not_found ->
            Log.warn "Trying to send a piece to non-existent peer"
        | p ->
            Peer.piece p idx ofs buf
        end

    | PeerEvent (_, Peer.BlockReceived (i, off, s)) ->
        if i >= 0 && i < Array.length pieces then
          record_block store peers pieces i off s bt.push
        else
          Log.warn "! PIECE invalid piece:%d" i

    | PeerEvent (_, Peer.GotPEX (added, dropped)) ->
        List.iter (fun (addr, _) -> PeerMgr.add bt.peer_mgr addr) added

    | PeerEvent (_, Peer.DHTPort _) ->
        (* let addr, _ = Peer.addr p in *)
        (* Lwt.async begin fun () -> *)
        (*   DHT.ping bt.dht (addr, i) >|= function *)
        (*   | Some (id, addr) -> *)
        (*       DHT.update bt.dht Kademlia.Good id addr *)
        (*   | None -> *)
        (*       debug "%s did not reply to dht ping on port %d" (Peer.to_string p) i *)
        (* end; *)
        (* FIXME *)
        ()

    | ConnectToPeer (addr, timeout) ->
        connect_to_peer ~id:bt.id ~info_hash:bt.ih addr timeout bt.push

    | IncomingPeer _ ->
        (* FIXME FIXME *)
        ()

    (* | HandshakeOk (addr, sock, exts, id) -> *)
    (*     PeerMgr.handshake_ok bt.peer_mgr addr id; *)
    (*     let p = welcome bt.push sock exts id in *)
    (*     Peer.have_bitfield p have; *)
    (*     (\* Hashtbl.replace peers id p; *\) *)
    (*     update_interest pieces p; *)
    (*     update_requests pieces peers *)

    | PeerEvent (_, Peer.AvailableMetadata _) ->
        ()
  in
  let rec loop () =
    Lwt.bind (Lwt_stream.next bt.chan) (fun e ->
        match handle e with
        | exception Exit ->
            Lwt.return_unit
        | exception e ->
            Lwt.fail e
        | () ->
            loop ()
      )
  in
  loop ()

let make_piece m i =
  { state = Pending;
    length = Metadata.piece_length m i;
    offset = Metadata.offset m i 0;
    have = 0;
    hash = Metadata.hash m i }

let load_torrent bt m =
  Store.create (Metadata.files m) >>= fun store ->
  let pieces = Array.init (Metadata.piece_count m) (make_piece m) in
  let have = Bits.create (Metadata.piece_count m) in
  let rec loop i =
    if i < Metadata.piece_count m then begin
      Store.digest store pieces.(i).offset pieces.(i).length >>= fun sha ->
      if SHA1.equal sha pieces.(i).hash then begin
        pieces.(i).state <- Verified;
        Bits.set have i
      end;
      loop (i + 1)
    end else
      Lwt.return (store, pieces, have)
  in
  loop 0

let rec fetch_metadata bt =
  (* let peers = Hashtbl.create 20 in *)
  (* let peer id f = try let p = Hashtbl.find peers id in f p with Not_found -> () in *)
  let rec loop m =
    Lwt_stream.next bt.chan >>= fun e ->
    (* log_event e; *)
    match m, e with
    | _, PeersReceived addrs ->
        List.iter (PeerMgr.add bt.peer_mgr) addrs;
        loop m

    | _, HandshakeOk (addr, sock, exts, id) ->
        PeerMgr.handshake_ok bt.peer_mgr addr id;
        let p = welcome bt.push sock exts id in
        (* Hashtbl.replace peers id p; *)
        loop m

    | _, HandshakeFailed addr ->
        PeerMgr.handshake_failed bt.peer_mgr addr;
        loop m

    | _, ConnectToPeer (addr, timeout) ->
        connect_to_peer ~id:bt.id ~info_hash:bt.ih addr timeout bt.push;
        loop m

    | _, IncomingPeer _ ->
        (* FIXME TODO FIXME *)
        loop m

    | _, PeerEvent (id, Peer.PeerDisconnected _) ->
        PeerMgr.peer_disconnected bt.peer_mgr id;
        (* Hashtbl.remove peers id; *)
        loop m

    | None, PeerEvent (id, Peer.AvailableMetadata len) ->
        if len <= IncompleteMetadata.metadata_max_size then
          let m' = IncompleteMetadata.create len in
          peer id (fun p -> IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m');
          loop (Some m')
        else begin
          Log.warn "! METADATA length %d is too large, ignoring." len;
          loop None
        end

    | Some m', PeerEvent (id, Peer.AvailableMetadata len) ->
        peer id (fun p -> IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m');
        loop m

    | _, PeerEvent (id, Peer.MetaRequested i) ->
        peer id (fun p -> Peer.reject_metadata_request p i);
        loop m

    | Some m', PeerEvent (_, Peer.GotMetaPiece (i, s)) ->
        if IncompleteMetadata.add m' i s then
          match IncompleteMetadata.verify m' bt.ih with
          | Some raw ->
              (* debug "got full metadata"; *)
              let m' = Metadata.create (Bcode.decode raw) in
              Lwt.return (m', peers)
          | None ->
              Log.error "METADATA HASH CHECK FAILED";
              loop None
        else
          loop m

    | None, PeerEvent (_, Peer.GotMetaPiece _) ->
        loop m

    | _, PeerEvent (_, Peer.GotPEX (added, dropped)) ->
        (* debug "got pex from %s added %d dropped %d" (Peer.to_string p) *)
        (*   (List.length added) (List.length dropped); *)
        List.iter (fun (addr, _) -> PeerMgr.add bt.peer_mgr addr) added;
        loop m

    | _, PeerEvent (_, Peer.DHTPort i) ->
        (* debug "got dht port %d from %s" i (Peer.to_string p); *)
        (* let addr, _ = Peer.addr p in *)
        (* Lwt.async begin fun () -> *)
        (*   DHT.ping bt.dht (addr, i) >|= function *)
        (*   | Some (id, addr) -> *)
        (*       DHT.update bt.dht Kademlia.Good id addr *)
        (*   | None -> *)
        (*       debug "%s did not reply to dht ping on port %d" (Peer.to_string p) i *)
        (* end *)
        (* FIXME *)
        loop m

    | _, PieceVerified _
    | _, PieceFailed _
    | _, BlockReadyToSend _
    | _, Error _
    | _, Rechoke _
    | _, TorrentComplete ->
        assert false

    | _, PeerEvent (_, Peer.Unchoked)
    | _, PeerEvent (_, Peer.Choked _)
    | _, PeerEvent (_, Peer.Interested)
    | _, PeerEvent (_, Peer.NotInterested)
    | _, PeerEvent (_, Peer.Have _)
    | _, PeerEvent (_, Peer.HaveBitfield _)
    | _, PeerEvent (_, Peer.BlockRequested _)
    | _, PeerEvent (_, Peer.BlockReceived _) ->
        loop m
  in
  loop None

module LPD  = struct

  module Log = L.Make (struct let section = "[LPD]" end)

  let mcast_addr = "239.192.152.143"
  let mcast_port = 6771

  open Cohttp

  module Http = Request.Make (String_io.M)

  let template =
    Printf.sprintf
      "BT-SEARCH * HTTP/1.1\r\n\
       Host: %s:%u\r\n\
       Port: %u\r\n\
       Infohash: %a\r\n\
       \r\n\r\n" mcast_addr mcast_port

  let get h name =
    match Header.get h name with
    | None -> Printf.ksprintf invalid_arg "Header not found : %S" name
    | Some s -> s

  let str = Bytes.create 200

  let start fd info_hash push =
    let rec loop () =
      Lwt_unix.recvfrom fd str 0 (Bytes.length str) [] >>= fun (n, sa) ->
      let ip, port = match sa with Unix.ADDR_INET (ip, port) -> ip, port | _ -> assert false in
      let buf = { String_io.str; pos = 0; len = n } in
      match Http.read buf with
      | `Ok r when r.Request.meth = `Other "BT-SEARCH" ->
          let ih = get r.Request.headers "Infohash" in
          let p = get r.Request.headers "Port" in
          Log.info "Received announce from = %s:%d ih = %s listen = %s"
            (Unix.string_of_inet_addr ip) port ih p;
          let ih = SHA1.of_raw @@ Cstruct.of_string @@ ih in
          push (ih, (ip, int_of_string p));
          loop ()
      | `Ok r ->
          Log.error "Unxpected HTTP method : %S" (Code.string_of_method r.Request.meth);
          loop ()
      | `Invalid s ->
          Log.error "Invalid HTTP request : %S" s;
          loop ()
      | `Eof ->
          Log.error "Unexpected end of file in HTTP request ";
          loop ()
    in
    loop ()

  let start fd info_hash push =
    Lwt.catch
      (fun () -> start fd info_hash push)
      (fun e ->
         Lwt.wrap2 Log.error "loop exn : %S ; restarting ..." (Printexc.to_string e))

  let start fd info_hash push =
    let rec loop () = start fd info_hash push >>= loop in
    loop ()

  let announce_delay = 5. *. 60.

  let announce fd port info_hash =
    let msg = template port SHA1.sprint_hex info_hash in
    let sa = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string mcast_addr, mcast_port) in
    let rec loop () =
      Lwt_unix.sendto fd msg 0 (String.length msg) [] sa >>= fun _ ->
      Lwt_unix.sleep announce_delay >>=
      loop
    in
    loop ()

  let announce fd port info_hash =
    Lwt.catch
      (fun () -> announce fd port info_hash)
      (fun e ->
         Lwt.wrap2 Log.error "announce exn : %s ; restarting ..." (Printexc.to_string e))

  let announce fd port info_hash =
    let rec loop () = announce fd port info_hash >>= loop in
    loop ()

  let start ~port ~info_hash push =
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    let fd2 = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
    Lwt_unix.setsockopt fd2 Lwt_unix.SO_REUSEADDR true;
    (* Lwt_unix.mcast_set_loop fd2 false; *)
    Lwt_unix.bind fd (Lwt_unix.ADDR_INET (Unix.inet_addr_any, mcast_port));
    Log.debug "Joining multicast group %s:%d" mcast_addr mcast_port;
    (* Lwt_unix.mcast_add_membership fd (Unix.inet_addr_of_string mcast_addr); *)
    Lwt.ignore_result (start fd info_hash push);
    Lwt.ignore_result (announce fd2 port info_hash)

end

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
    push (IncomingPeer (addr, new Util.tcp fd));
    loop ()
  in
  loop ()

let start_server ?port push =
  Lwt.ignore_result (start_server ?port push)

let start bt =
  start_server bt.push;
  List.iter (Tracker.announce ~info_hash:bt.ih (fun peers -> bt.push (PeersReceived peers)) bt.id) bt.trackers;
  fetch_metadata bt >>=
  load_torrent bt >>= fun (store, pieces, have) ->
  Log.info "Torrent loaded have %d/%d pieces" (Bits.count_ones have) (Bits.length have);
  share_torrent bt m store pieces have peers

let create mg =
  let ch, push = let ch, push = Lwt_stream.create () in ch, (fun x -> push (Some x)) in
  let peer_mgr = PeerMgr.create (fun addr timeout -> push @@ ConnectToPeer (addr, timeout)) in
  { id = SHA1.generate ~prefix:"OCAML" ();
    ih = mg.Magnet.xt;
    trackers = mg.Magnet.tr;
    chan = ch;
    push;
    peer_mgr }
