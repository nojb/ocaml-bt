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

let section = Log.make_section "Handshake"

let debug ?exn fmt = Log.debug section ?exn fmt
    
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let vc = String.make 8 '\x00'    
let crypto_plain = 0x01l
let crypto_rc4 = 0x02l
let crypto_provide = crypto_rc4
let max_pad_len = 512
let key_len = 96

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

let fail_lwt fmt =
  Printf.ksprintf (fun s -> Lwt.fail (Failure s)) fmt

let dh_prime =
  "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xC9\x0F\xDA\xA2\
   \x21\x68\xC2\x34\xC4\xC6\x62\x8B\x80\xDC\x1C\xD1\
   \x29\x02\x4E\x08\x8A\x67\xCC\x74\x02\x0B\xBE\xA6\
   \x3B\x13\x9B\x22\x51\x4A\x08\x79\x8E\x34\x04\xDD\
   \xEF\x95\x19\xB3\xCD\x3A\x43\x1B\x30\x2B\x0A\x6D\
   \xF2\x5F\x14\x37\x4F\xE1\x35\x6D\x6D\x51\xC2\x45\
   \xE4\x85\xB5\x76\x62\x5E\x7E\xC6\xF4\x4C\x42\xE9\
   \xA6\x3A\x36\x21\x00\x00\x00\x00\x00\x09\x05\x63"

let dh_params =
  { Cryptokit.DH.p = dh_prime;
    g = "\x02";
    privlen = 160 }

type crypto_mode =
  | Require
  | Prefer

type encryption_mode =
  | Crypto of crypto_mode
  | Plain

let crypto_select mode crypto_provide =
  match mode, crypto_provide with
  | Plain, 0x01l
  | Plain, 0x03l -> Lwt.return 0x01l
  | Crypto Require, 0x2l
  | Crypto Require, 0x3l 
  | Crypto Prefer, 0x2l
  | Crypto Prefer, 0x3l -> Lwt.return 0x2l
  | Crypto Prefer, 0x1l -> Lwt.return 0x1l
  | _ -> Lwt.fail (Failure "no suitable crypto_select")

let crypto_provide = function
  | Plain -> 0x01l
  | Crypto Require -> 0x02l
  | Crypto Prefer -> 0x03l

type result =
  | Success of SHA1.t * Bits.t
  | Failed

type handshake_callback = result -> unit

type t = {
  private_key : Cryptokit.DH.private_secret;
  skey : SHA1.t;
  incoming : bool;
  id : SHA1.t;
  mode : encryption_mode;
  callback : handshake_callback;
  sock : IO.t;
  on_abort : unit Lwt_condition.t
}

let to_string hs =
  Printf.sprintf "%s [%s]"
    (Addr.to_string (IO.addr hs.sock))
    (if hs.incoming then "incoming" else "outgoing")

let keyA = "keyA"
  
let keyB = "keyB"

let arcfour_key from secret skey =
  let key = SHA1.strings [from; secret; SHA1.to_bin skey] in
  SHA1.to_bin key

let arcfour =
  let discard = String.create 1024 in
  fun key ->
    let c = new Cryptokit.Stream.arcfour key in
    c#transform discard 0 discard 0 (String.length discard);
    c

(*
1 A->B: Diffie Hellman Ya, PadA
2 B->A: Diffie Hellman Yb, PadB
3 A->B: HASH('req1', S), HASH('req2', SKEY) xor HASH('req3', S), ENCRYPT(VC, crypto_provide, len(PadC), PadC, len(IA)), ENCRYPT(IA)
4 B->A: ENCRYPT(VC, crypto_select, len(padD), padD), ENCRYPT2(Payload Stream)
5 A->B: ENCRYPT2(Payload Stream)
*)

let proto = "BitTorrent protocol"

let extended_bits =
  let bits = Bits.create (8 * 8) in
  Bits.set bits Wire.ltep_bit;
  Bits.set bits Wire.dht_bit;
  bits

let handshake_message id ih =
  let bs =
    BITSTRING
      { 19 : 8; proto : -1 : string;
        Bits.to_bin extended_bits : -1 : string;
        SHA1.to_bin ih : 20 * 8 : string;
        SHA1.to_bin id : 20 * 8 : string }
  in
  Bitstring.string_of_bitstring bs

let handshake_len = 49 + String.length proto

let send_handshake hs =
  IO.write_string hs.sock (handshake_message hs.id hs.skey)

let parse_handshake hs str =
  bitmatch Bitstring.bitstring_of_string str with
  | { 19 : 8;
      proto : 19 * 8 : string;
      extbits : 8 * 8 : string, bind (Bits.of_bin extbits);
      ih : 20 * 8 : string, bind (SHA1.from_bin ih);
      id : 20 * 8 : string, bind (SHA1.from_bin id) } ->
    if SHA1.equal hs.skey ih then
      Lwt.return (id, extbits)
    else
      fail_lwt "info-hash mismatch received:%s expected:%s" (SHA1.to_hex ih) (SHA1.to_hex hs.skey)

let read_handshake hs =
  IO.read_string hs.sock handshake_len >>= parse_handshake hs

let send_random_padding hs =
  let len = Random.int (max_pad_len + 1) in
  let pad = Cryptokit.Random.string Cryptokit.Random.secure_rng len in
  IO.write_string hs.sock pad

let sync_and_read_vc hs shared_secret =
  let buf = String.create (String.length vc) in
  let buf1 = String.create (String.length vc) in
  let key = arcfour_key keyB shared_secret hs.skey in
  let rec loop p =
    if p >= max_pad_len then
      fail_lwt "could not find vc"
    else begin
      let c = arcfour key in
      c#transform buf 0 buf1 0 (String.length vc);
      if buf1 = vc then
        Lwt.return c
      else begin
        String.blit buf 1 buf 0 (String.length vc - 1);
        IO.really_read hs.sock buf (String.length vc - 1) 1 >>= fun () ->
        loop (p+1)
      end
    end
  in
  IO.really_read hs.sock buf 0 (String.length vc) >>= fun () ->
  loop 0

let skip_pad hs =
  IO.read_int16 hs.sock >>= fun pad_len ->
  if 0 <= pad_len && pad_len <= max_pad_len then
    IO.read_string hs.sock pad_len >>= fun _ ->
    Lwt.return ()
  else
    Lwt.fail (Failure (Printf.sprintf "pad len too long(%d)" pad_len))

let read_dh_key hs =
  IO.read_string hs.sock key_len >>= fun yb ->
  Lwt.return yb

let req2_xor_req3 hs shared_secret =
  let req2 = (SHA1.strings ["req2"; SHA1.to_bin hs.skey] :> string) in
  let req3 = (SHA1.strings ["req3"; shared_secret] :> string) in
  Cryptokit.xor_string req2 0 req3 0 (String.length req2);
  req3

let send_payload hs shared_secret =
  IO.write_string hs.sock (SHA1.strings ["req1"; shared_secret] :> string) >>= fun () ->
  IO.write_string hs.sock (req2_xor_req3 hs shared_secret) >>= fun () ->
  let key = arcfour_key keyA shared_secret hs.skey in
  let arc4encrypt = arcfour key in
  IO.enable_encryption hs.sock arc4encrypt;
  IO.write_string hs.sock vc >>= fun () ->
  let crypto_provide = crypto_provide hs.mode in
  IO.write_int32 hs.sock crypto_provide >>= fun () ->
  debug "sent crypto_provide(%lX) to %s" crypto_provide (to_string hs);
  IO.write_int16 hs.sock 0 >>= fun () ->
  let m = handshake_message hs.id hs.skey in
  IO.write_int16 hs.sock (String.length m) >>= fun () ->
  IO.write_string hs.sock m

let send_dh_key hs =
  let y = Cryptokit.DH.message dh_params hs.private_key in
  IO.write_string hs.sock y >>= fun () ->
  let pad_len = Random.int (max_pad_len + 1) in
  let pad = Cryptokit.Random.string Cryptokit.Random.secure_rng pad_len in
  IO.write_string hs.sock pad

let run_outgoing hs =
  match hs.mode with
  | Plain ->
    send_handshake hs >>= fun () ->
    read_handshake hs >|= fun (id, exts) ->
    hs.callback (Success (id, exts))
  | Crypto mode ->
    let require = match mode with Require -> true | Prefer -> false in
    Lwt.catch begin fun () ->
      send_dh_key hs >>= fun () ->
      read_dh_key hs >>= fun yb ->
      let shared_secret = Cryptokit.DH.shared_secret dh_params hs.private_key yb in
      send_payload hs shared_secret >>= fun () ->
      sync_and_read_vc hs shared_secret >>= fun dec ->
      IO.enable_decryption hs.sock dec;
      IO.read_int32 hs.sock >>= fun crypto_select ->
      skip_pad hs >>= fun () ->
      match crypto_select with
      | 0x01l (* PLAIN *) ->
        if require then Lwt.fail (Failure "crypto required")
        else begin
          IO.disable_encryption hs.sock;
          IO.disable_decryption hs.sock;
          read_handshake hs >|= fun (id, exts) ->
          hs.callback (Success (id, exts))
        end
      | 0x02l (* RC4 *) ->
        read_handshake hs >|= fun (id, exts) ->
        hs.callback (Success (id, exts))
      | _ ->
        Lwt.fail (Failure (Printf.sprintf "unknown crypto_select(%lX)" crypto_select))
    end begin fun e ->
      if require then
        Lwt.fail e
      else
        IO.reconnect hs.sock >>= fun () ->
        send_handshake hs >>= fun () ->
        read_handshake hs >|= fun (id, exts) ->
        hs.callback (Success (id, exts))
    end
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

let sync_and_read_req1 hs shared_secret =
  let buf = String.create 20 in
  let rec loop i =
    if i >= max_pad_len then Lwt.fail (Failure "HASH('req1', S) not found") else
    if buf = SHA1.to_bin (SHA1.strings ["req1"; shared_secret]) then Lwt.return ()
    else begin
      String.blit buf 1 buf 0 19;
      IO.really_read hs.sock buf 19 1 >>= fun () ->
      loop (i+1)
    end
  in
  loop 0

let read_req2_xor_req3 hs shared_secret =
  IO.read_string hs.sock 20 >>= fun s ->
  if s = req2_xor_req3 hs shared_secret then Lwt.return ()
  else Lwt.fail (Failure "expected HASH('req2', SKEY) xor HASH('req3', S)")

let read_vc hs =
  IO.read_string hs.sock (String.length vc) >>= fun s ->
  if s = vc then Lwt.return () else Lwt.fail (Failure "missing ENCRYPT(VC)")

let read_crypto_provide hs shared_secret =
  sync_and_read_req1 hs shared_secret >>= fun () ->
  read_req2_xor_req3 hs shared_secret >>= fun () ->
  let key = arcfour_key keyA shared_secret hs.skey in
  let c = arcfour key in
  IO.enable_decryption hs.sock c;
  read_vc hs >>= fun () ->
  IO.read_int32 hs.sock

let read_ia_into hs m ofs =
  IO.read_int16 hs.sock >>= fun ia_len ->
  IO.really_read hs.sock m ofs ia_len >>= fun () ->
  Lwt.return ia_len

let assert_crypto crypto_provide mode =
  match mode, crypto_provide with
  | Require, 0x2l
  | Require, 0x3l ->
    Lwt.return ()
  | Require, 0x1l ->
    Lwt.fail
      (Failure (Printf.sprintf "crypto_provide(%lX) is not RC4(%lX)" crypto_provide crypto_rc4))
  | Prefer, 0x1l
  | Prefer, 0x2l
  | Prefer, 0x3l ->
    Lwt.return ()
  | _ ->
    Lwt.fail (Failure (Printf.sprintf "unknown crypto_provide(%lX)" crypto_provide))

let run_incoming hs =
  match hs.mode with
  | Plain ->
    read_handshake hs >>= fun (id, exts) ->
    send_handshake hs >|= fun () ->
    hs.callback (Success (id, exts))
  | Crypto mode ->
    let require = match mode with Require -> true | Prefer -> false in
    Lwt.catch begin fun () ->
      read_dh_key hs >>= fun ya ->
      send_dh_key hs >>= fun () ->
      let shared_secret = Cryptokit.DH.shared_secret dh_params hs.private_key ya in
      read_crypto_provide hs shared_secret >>= fun crypto_provide ->
      assert_crypto crypto_provide mode >>= fun () ->
      skip_pad hs >>= fun () ->
      let m = String.create handshake_len in
      read_ia_into hs m 0 >>= fun ia_len ->
      crypto_select hs.mode crypto_provide >>= fun crypto_select ->
      let key = arcfour_key keyB shared_secret hs.skey in
      let c = arcfour key in
      IO.enable_encryption hs.sock c;
      IO.write_string hs.sock vc >>= fun () ->
      IO.write_int32 hs.sock crypto_select >>= fun () ->
      send_random_padding hs >>= fun () ->
      if crypto_select = crypto_plain then begin
        IO.disable_encryption hs.sock;
        IO.disable_decryption hs.sock
      end;
      let d = handshake_len - ia_len in
      IO.really_read hs.sock m ia_len d >>= fun () ->
      parse_handshake hs m >|= fun (id, exts) ->
      hs.callback (Success (id, exts))
    end begin fun e ->
      if require then
        Lwt.fail e
      else (* FIXME *)
        IO.reconnect hs.sock >>= fun () ->
        send_handshake hs >>= fun () ->
        read_handshake hs >|= fun (id, exts) ->
        hs.callback (Success (id, exts))
    end
  (* | Crypto mode -> *)
  (*   Lwt.catch begin fun () -> *)
  (*     read_dh_key hs >>= fun ya -> *)
  (*     let shared_secret = Cryptokit.DH.shared_secret dh_params hs.private_key ya in *)
  (*     send_dh_key hs >>= fun () -> *)
  (*     read_crypto_provide hs shared_secret >>= fun crypto_provide -> *)
  (*     skip_pad_c hs >>= fun () -> *)
  (*     let m = String.create handshake_len in *)
  (*     IO.read_int16 hs.sock >>= fun ia_len -> *)
  (*     IO.really_read hs.sock m 0 ia_len >>= fun () -> *)
  (*     assert_crypto crypto_provide crypto_rc4 >>= fun () -> *)
  (*     send_vc_crypto_select_pad_d hs >>= fun () -> *)
  (*     let ia_len = String.length ia in *)
  (*     let d = handshake_len - ia_len in *)
  (*     IO.really_read hs.sock m ia_len d >>= fun () -> *)
  (*     parse_handshake m >>= fun (id, exts) -> *)
  (*     hs.callback (Success (id, exts)) *)
  (*   end begin fun e -> *)
  (*     (\* Log.debug ~exn:e "error while trying crypto connection; retrying with plain..."; *\) *)
  (*     (\* IO.reconnect hs.sock >>= fun () -> *\) *)
  (*     (\* let m = handshake_message hs.id hs.skey in *\) *)
  (*     (\* IO.write_string hs.sock m >>= fun () -> *\) *)
  (*     (\* read_handshake hs >|= fun (id, exts) -> *\) *)
  (*     (\* hs.callback (Success (id, exts)) *\) *)
  (*   end *)

let create ~id ~ih mode sock callback incoming =
  { private_key = Cryptokit.DH.private_secret dh_params;
    incoming;
    skey = ih;
    id;
    mode;
    callback;
    sock;
    on_abort = Lwt_condition.create () }

exception Abort

let run hs f =
  Lwt.catch
    (fun () ->
       Lwt.pick [(Lwt_condition.wait hs.on_abort >>= fun () -> Lwt.fail Abort); f hs])
    (fun exn ->
       debug ~exn "error"; hs.callback Failed; Lwt.return ())

let outgoing ~id ~ih mode sock callback =
  let hs = create ~id ~ih mode sock callback false in
  Lwt.async (fun () -> run hs run_outgoing);
  hs

let incoming ~id ~ih mode sock callback =
  let hs = create ~id ~ih mode sock callback true in
  Lwt.async (fun () -> run hs run_incoming);
  hs

let abort hs =
  Lwt_condition.broadcast hs.on_abort ()

let addr hs =
  IO.addr hs.sock
