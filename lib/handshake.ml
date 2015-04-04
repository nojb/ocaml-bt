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

module S    = SHA1
module Dh   = Nocrypto.Dh
module Z    = Nocrypto.Numeric.Z
module ARC4 = Nocrypto.Cipher_stream.ARC4
module SHA1 = Nocrypto.Hash.SHA1
module Cs   = Nocrypto.Uncommon.Cs
module Rng  = Nocrypto.Rng

let _ = Nocrypto.Rng.reseed (Cstruct.of_string "fadsfadsfadsfdasF")

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

let priv, pub = Dh.gen_secret g

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

let discard = Cstruct.create 1024

let arc4 from secret skey =
  let key = ARC4.of_secret @@ SHA1.digestv [ from; secret; skey ] in
  let { ARC4.key } = ARC4.encrypt ~key discard in
  key

(*
1 A->B: Diffie Hellman Ya, PadA
2 B->A: Diffie Hellman Yb, PadB
3 A->B: HASH('req1', S), HASH('req2', SKEY) xor HASH('req3', S), ENCRYPT(VC, crypto_provide, len(PadC), PadC, len(IA)), ENCRYPT(IA)
4 B->A: ENCRYPT(VC, crypto_select, len(padD), padD), ENCRYPT2(Payload Stream)
5 A->B: ENCRYPT2(Payload Stream)
*)

(* let send_random_padding hs = *)
(*   let len = Random.int (max_pad_len + 1) in *)
(*   let pad = Cryptokit.Random.string Cryptokit.Random.secure_rng len in *)
(*   IO.write_string hs.sock pad *)

(* let sync_and_read_vc hs shared_secret = *)
(*   let buf = String.create (String.length vc) in *)
(*   let buf1 = String.create (String.length vc) in *)
(*   let key = arcfour_key keyB shared_secret hs.skey in *)
(*   let rec loop p = *)
(*     if p >= max_pad_len then *)
(*       fail_lwt "could not find vc" *)
(*     else begin *)
(*       let c = arcfour key in *)
(*       c#transform buf 0 buf1 0 (String.length vc); *)
(*       if buf1 = vc then *)
(*         Lwt.return c *)
(*       else begin *)
(*         String.blit buf 1 buf 0 (String.length vc - 1); *)
(*         IO.really_read hs.sock buf (String.length vc - 1) 1 >>= fun () -> *)
(*         loop (p+1) *)
(*       end *)
(*     end *)
(*   in *)
(*   IO.really_read hs.sock buf 0 (String.length vc) >>= fun () -> *)
(*   loop 0 *)

(* let skip_pad hs = *)
(*   IO.read_int16 hs.sock >>= fun pad_len -> *)
(*   if 0 <= pad_len && pad_len <= max_pad_len then *)
(*     IO.read_string hs.sock pad_len >>= fun _ -> *)
(*     Lwt.return () *)
(*   else *)
(*     Lwt.fail (Failure (Printf.sprintf "pad len too long(%d)" pad_len)) *)

(* let read_dh_key hs = *)
(*   IO.read_string hs.sock key_len >>= fun yb -> *)
(*   Lwt.return yb *)

(* let req2_xor_req3 hs shared_secret = *)
(*   let req2 = (SHA1.strings ["req2"; SHA1.to_bin hs.skey] :> string) in *)
(*   let req3 = (SHA1.strings ["req3"; shared_secret] :> string) in *)
(*   Cryptokit.xor_string req2 0 req3 0 (String.length req2); *)
(*   req3 *)

(* let send_payload hs shared_secret = *)
(*   IO.write_string hs.sock (SHA1.strings ["req1"; shared_secret] :> string) >>= fun () -> *)
(*   IO.write_string hs.sock (req2_xor_req3 hs shared_secret) >>= fun () -> *)
(*   let key = arcfour_key keyA shared_secret hs.skey in *)
(*   let arc4encrypt = arcfour key in *)
(*   IO.enable_encryption hs.sock arc4encrypt; *)
(*   IO.write_string hs.sock vc >>= fun () -> *)
(*   let crypto_provide = crypto_provide hs.mode in *)
(*   IO.write_int32 hs.sock crypto_provide >>= fun () -> *)
(*   debug "sent crypto_provide(%lX) to %s" crypto_provide (to_string hs); *)
(*   IO.write_int16 hs.sock 0 >>= fun () -> *)
(*   let m = handshake_message hs.id hs.skey in *)
(*   IO.write_int16 hs.sock (String.length m) >>= fun () -> *)
(*   IO.write_string hs.sock m *)

(* let send_dh_key hs = *)
(*   let y = Cryptokit.DH.message dh_params hs.private_key in *)
(*   IO.write_string hs.sock y >>= fun () -> *)
(*   let pad_len = Random.int (max_pad_len + 1) in *)
(*   let pad = Cryptokit.Random.string Cryptokit.Random.secure_rng pad_len in *)
(*   IO.write_string hs.sock pad *)

let cs_int16 n =
  let cs = Cstruct.create 2 in
  Cstruct.BE.set_uint16 cs 0 n;
  cs

let cs_int32 n =
  let cs = Cstruct.create 4 in
  Cstruct.BE.set_uint32 cs 0 n;
  cs

let handle_client t state =
  let rec loop buf st =
    let len = Cstruct.len buf in
    match st with
    | ClientWaitDh when len >= Dh.apparent_bit_size g ->
        let yb, buf = Cstruct.split buf (Dh.apparent_bit_size g) in
        let shared_secret = Dh.shared g priv yb in
        let my_key = arc4 keyA shared_secret t.info_hash in
        let her_key = arc4 keyB shared_secret t.info_hash in
        let padC_len = Rng.Int.gen (max_pad_len + 1) in
        let padC = Rng.generate padC_len in
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

let handle_server _ =
  assert false

let (<+>) = Cs.(<+>)

let handle t buf =
  let t = { t with buf = t.buf <+> buf } in
  match t.state with
  | Client state ->
      handle_client t state
  | Server state ->
      handle_server t state

  (* (\* match hs.mode with *\) *)
  (* (\* | Plain -> *\) *)
  (* (\*   send_handshake hs >>= fun () -> *\) *)
  (* (\*   read_handshake hs >|= fun (id, exts) -> *\) *)
  (* (\*   hs.callback (Success (id, exts)) *\) *)
  (* (\* | Crypto mode -> *\) *)
  (*   let require = match mode with Require -> true | Prefer -> false in *)
  (*   Lwt.catch begin fun () -> *)
  (*     send_dh_key hs >>= fun () -> *)
  (*     read_dh_key hs >>= fun yb -> *)
  (*     let shared_secret = Cryptokit.DH.shared_secret dh_params hs.private_key yb in *)
  (*     send_payload hs shared_secret >>= fun () -> *)
  (*     sync_and_read_vc hs shared_secret >>= fun dec -> *)
  (*     IO.enable_decryption hs.sock dec; *)
  (*     IO.read_int32 hs.sock >>= fun crypto_select -> *)
  (*     skip_pad hs >>= fun () -> *)
  (*     match crypto_select with *)
  (*     | 0x01l (\* PLAIN *\) -> *)
  (*       if require then Lwt.fail (Failure "crypto required") *)
  (*       else begin *)
  (*         IO.disable_encryption hs.sock; *)
  (*         IO.disable_decryption hs.sock; *)
  (*         read_handshake hs >|= fun (id, exts) -> *)
  (*         hs.callback (Success (id, exts)) *)
  (*       end *)
  (*     | 0x02l (\* RC4 *\) -> *)
  (*       read_handshake hs >|= fun (id, exts) -> *)
  (*       hs.callback (Success (id, exts)) *)
  (*     | _ -> *)
  (*       Lwt.fail (Failure (Printf.sprintf "unknown crypto_select(%lX)" crypto_select)) *)
  (*   end begin fun e -> *)
  (*     if require then *)
  (*       Lwt.fail e *)
  (*     else *)
  (*       IO.reconnect hs.sock >>= fun () -> *)
  (*       send_handshake hs >>= fun () -> *)
  (*       read_handshake hs >|= fun (id, exts) -> *)
  (*       hs.callback (Success (id, exts)) *)
  (*   end *)
        (* | PreferCrypto -> *)
  (*   Lwt.catch begin fun () -> *)
  (*     send_dh_key hs >>= fun () -> *)
  (*     read_dh_key hs >>= fun yb -> *)
  (*     let shared_secret = Cryptokit.DH.shared_secret dh_params hs.private_key yb in *)
  (*     send_payload hs shared_secret >>= fun () -> *)
  (*     read_vc hs shared_secret >>= fun _ -> *)
  (*     read_handshake hs >|= fun (id, exts) -> *)
  (*     hs.callback (Success (id, exts)) *)
  (*   end begin fun e -> *)
  (*     Log.debug ~exn:e "error while trying crypto connection; retrying with plain..."; *)
  (*     IO.reconnect hs.sock >>= fun () -> *)
  (*     let m = handshake_message hs.id hs.skey in *)
  (*     IO.write_string hs.sock m >>= fun () -> *)
  (*     read_handshake hs >|= fun (id, exts) -> *)
  (*     hs.callback (Success (id, exts)) *)
  (*   end *)

(* let sync_and_read_req1 hs shared_secret = *)
(*   let buf = String.create 20 in *)
(*   let rec loop i = *)
(*     if i >= max_pad_len then Lwt.fail (Failure "HASH('req1', S) not found") else *)
(*     if buf = SHA1.to_bin (SHA1.strings ["req1"; shared_secret]) then Lwt.return () *)
(*     else begin *)
(*       String.blit buf 1 buf 0 19; *)
(*       IO.really_read hs.sock buf 19 1 >>= fun () -> *)
(*       loop (i+1) *)
(*     end *)
(*   in *)
(*   loop 0 *)

(* let read_req2_xor_req3 hs shared_secret = *)
(*   IO.read_string hs.sock 20 >>= fun s -> *)
(*   if s = req2_xor_req3 hs shared_secret then Lwt.return () *)
(*   else Lwt.fail (Failure "expected HASH('req2', SKEY) xor HASH('req3', S)") *)

(* let read_vc hs = *)
(*   IO.read_string hs.sock (String.length vc) >>= fun s -> *)
(*   if s = vc then Lwt.return () else Lwt.fail (Failure "missing ENCRYPT(VC)") *)

(* let read_crypto_provide hs shared_secret = *)
(*   sync_and_read_req1 hs shared_secret >>= fun () -> *)
(*   read_req2_xor_req3 hs shared_secret >>= fun () -> *)
(*   let key = arcfour_key keyA shared_secret hs.skey in *)
(*   let c = arcfour key in *)
(*   IO.enable_decryption hs.sock c; *)
(*   read_vc hs >>= fun () -> *)
(*   IO.read_int32 hs.sock *)

(* let read_ia_into hs m ofs = *)
(*   IO.read_int16 hs.sock >>= fun ia_len -> *)
(*   IO.really_read hs.sock m ofs ia_len >>= fun () -> *)
(*   Lwt.return ia_len *)

(* let assert_crypto crypto_provide mode = *)
(*   match mode, crypto_provide with *)
(*   | Require, 0x2l *)
(*   | Require, 0x3l -> *)
(*     Lwt.return () *)
(*   | Require, 0x1l -> *)
(*     Lwt.fail *)
(*       (Failure (Printf.sprintf "crypto_provide(%lX) is not RC4(%lX)" crypto_provide crypto_rc4)) *)
(*   | Prefer, 0x1l *)
(*   | Prefer, 0x2l *)
(*   | Prefer, 0x3l -> *)
(*     Lwt.return () *)
(*   | _ -> *)
(*     Lwt.fail (Failure (Printf.sprintf "unknown crypto_provide(%lX)" crypto_provide)) *)

(* let run_incoming hs = *)
(*   match hs.mode with *)
(*   | Plain -> *)
(*     read_handshake hs >>= fun (id, exts) -> *)
(*     send_handshake hs >|= fun () -> *)
(*     hs.callback (Success (id, exts)) *)
(*   | Crypto mode -> *)
(*     let require = match mode with Require -> true | Prefer -> false in *)
(*     Lwt.catch begin fun () -> *)
(*       read_dh_key hs >>= fun ya -> *)
(*       send_dh_key hs >>= fun () -> *)
(*       let shared_secret = Cryptokit.DH.shared_secret dh_params hs.private_key ya in *)
(*       read_crypto_provide hs shared_secret >>= fun crypto_provide -> *)
(*       assert_crypto crypto_provide mode >>= fun () -> *)
(*       skip_pad hs >>= fun () -> *)
(*       let m = String.create handshake_len in *)
(*       read_ia_into hs m 0 >>= fun ia_len -> *)
(*       crypto_select hs.mode crypto_provide >>= fun crypto_select -> *)
(*       let key = arcfour_key keyB shared_secret hs.skey in *)
(*       let c = arcfour key in *)
(*       IO.enable_encryption hs.sock c; *)
(*       IO.write_string hs.sock vc >>= fun () -> *)
(*       IO.write_int32 hs.sock crypto_select >>= fun () -> *)
(*       send_random_padding hs >>= fun () -> *)
(*       if crypto_select = crypto_plain then begin *)
(*         IO.disable_encryption hs.sock; *)
(*         IO.disable_decryption hs.sock *)
(*       end; *)
(*       let d = handshake_len - ia_len in *)
(*       IO.really_read hs.sock m ia_len d >>= fun () -> *)
(*       parse_handshake hs m >|= fun (id, exts) -> *)
(*       hs.callback (Success (id, exts)) *)
(*     end begin fun e -> *)
(*       if require then *)
(*         Lwt.fail e *)
(*       else (\* FIXME *\) *)
(*         IO.reconnect hs.sock >>= fun () -> *)
(*         send_handshake hs >>= fun () -> *)
(*         read_handshake hs >|= fun (id, exts) -> *)
(*         hs.callback (Success (id, exts)) *)
(*     end *)
(*   (\* | Crypto mode -> *\) *)
(*   (\*   Lwt.catch begin fun () -> *\) *)
(*   (\*     read_dh_key hs >>= fun ya -> *\) *)
(*   (\*     let shared_secret = Cryptokit.DH.shared_secret dh_params hs.private_key ya in *\) *)
(*   (\*     send_dh_key hs >>= fun () -> *\) *)
(*   (\*     read_crypto_provide hs shared_secret >>= fun crypto_provide -> *\) *)
(*   (\*     skip_pad_c hs >>= fun () -> *\) *)
(*   (\*     let m = String.create handshake_len in *\) *)
(*   (\*     IO.read_int16 hs.sock >>= fun ia_len -> *\) *)
(*   (\*     IO.really_read hs.sock m 0 ia_len >>= fun () -> *\) *)
(*   (\*     assert_crypto crypto_provide crypto_rc4 >>= fun () -> *\) *)
(*   (\*     send_vc_crypto_select_pad_d hs >>= fun () -> *\) *)
(*   (\*     let ia_len = String.length ia in *\) *)
(*   (\*     let d = handshake_len - ia_len in *\) *)
(*   (\*     IO.really_read hs.sock m ia_len d >>= fun () -> *\) *)
(*   (\*     parse_handshake m >>= fun (id, exts) -> *\) *)
(*   (\*     hs.callback (Success (id, exts)) *\) *)
(*   (\*   end begin fun e -> *\) *)
(*   (\*     (\\* Log.debug ~exn:e "error while trying crypto connection; retrying with plain..."; *\\) *\) *)
(*   (\*     (\\* IO.reconnect hs.sock >>= fun () -> *\\) *\) *)
(*   (\*     (\\* let m = handshake_message hs.id hs.skey in *\\) *\) *)
(*   (\*     (\\* IO.write_string hs.sock m >>= fun () -> *\\) *\) *)
(*   (\*     (\\* read_handshake hs >|= fun (id, exts) -> *\\) *\) *)
(*   (\*     (\\* hs.callback (Success (id, exts)) *\\) *\) *)
(*   (\*   end *\) *)

type ret =
  [ `Ok of t * Cstruct.t option
  | `Success of (ARC4.key * ARC4.key) option * Cstruct.t
  | `Error of string ]

let outgoing ~info_hash mode =
  let info_hash = S.to_raw info_hash in
  let t = { info_hash; mode; state = Client ClientWaitDh; buf = Cs.empty } in
  let padA_len = Rng.Int.gen (max_pad_len + 1) in
  let padA = Rng.generate padA_len in
  `Ok (t, Some (pub <+> padA))

let incoming ~info_hash mode =
  `Error "not implemented"
