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

module Cs   = Nocrypto.Uncommon.Cs

module Encryption = struct

  module Dh   = Nocrypto.Dh
  module Z    = Nocrypto.Numeric.Z
  module ARC4 = Nocrypto.Cipher_stream.ARC4
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

type result =
  | Ok of Util.socket * Bits.t * SHA1.t
  | Failed

let with_socket f =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt.catch (fun () -> f fd)
    (fun e ->
       Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit) >>= fun () -> Lwt.fail e)

let outgoing kind ~id ~info_hash addr =
  let ip, port = addr in
  let f ~info_hash sock = match kind with
    | Encrypted ->
        Encryption.outgoing ~info_hash:(SHA1.to_raw info_hash) sock Encryption.Both
    | Plain ->
        Lwt.return (sock, Cs.empty)
  in
  let connect fd =
    Lwt_log.ign_notice_f "Connecting [%s] to %s:%d..." (match kind with Encrypted -> "ENCRYPTED" | Plain -> "PLAIN")
      (Unix.string_of_inet_addr ip) port;
    let sa = Lwt_unix.ADDR_INET (ip, port) in
    Lwt_unix.connect fd sa >>= fun () ->
    f ~info_hash (new Util.tcp fd) >>= fun (sock, rest) ->
    outgoing_handshake ~id ~info_hash sock rest
  in
  with_socket connect

let outgoing ~id ~info_hash addr push =
  Lwt.try_bind
    (fun () ->
       outgoing Plain ~id ~info_hash addr)
    (fun (sock, ext, peer_id) ->
       Lwt.wrap1 push @@ Ok (sock, ext, peer_id))
    (function
      | e ->
          Lwt_log.ign_error (Printexc.to_string e);
          Lwt.wrap1 push Failed) |> Lwt.ignore_result
      (* | Unix.Unix_error (_, "connect", _) -> *)
      (*     Lwt.wrap1 push Failed *)
      (* | _ -> *)
      (*     Log.error "Encrypted connection to %s:%d failed, retrying with plain..." *)
      (*       (Unix.string_of_inet_addr (fst addr)) (snd addr); *)
      (*     Lwt.try_bind (fun () -> outgoing Plain ~id ~info_hash addr) *)
      (*       (fun (sock, ext, peer_id) -> Lwt.wrap1 push @@ Ok (sock, ext, peer_id)) *)
      (*       (fun _ -> Lwt.wrap1 push Failed)) |> Lwt.ignore_result *)
