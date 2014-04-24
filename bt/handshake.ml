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

type t = {
  private_key : Cryptokit.DH.private_secret;
  skey : SHA1.t;
  incoming : bool;
  id : SHA1.t
}
type result =
  | Retry
  | Success of SHA1.t * Bits.t * IO.socket
  | Failure

let hash sl =
  let h = Cryptokit.Hash.sha1 () in
  List.iter h#add_string sl;
  h#result
    
let create ~incoming ~id ~ih =
  { private_key = Cryptokit.DH.private_secret dh_params;
    incoming;
    skey = ih;
    id }

let arcfour =
  let discard = String.create 1024 in
  fun ~incoming ~secret ~skey ->
    let key =
      if incoming then hash ["keyB"; secret; skey] else hash ["keyA"; secret; skey]
    in
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

(* let read_handshake sock = *)
(*   sock#read_string (49 + String.length proto) >>= fun str -> *)
(*   bitmatch Bitstring.bitstring_of_string str with *)
(*   | { 19 : 8; *)
(*       proto : 19 * 8 : string; *)
(*       extbits : 8 * 8 : string, bind (Bits.of_bin extbits); *)
(*       ih : 20 * 8 : string, bind (SHA1.from_bin ih); *)
(*       id : 20 * 8 : string, bind (SHA1.from_bin id) } -> *)
(*     Lwt.return (ih, id, extbits) *)

let extended_bits =
  let bits = Bits.create (8 * 8) in
  Bits.set bits Wire.lt_extension_bit;
  bits

let handshake_message id ih =
  BITSTRING
    { 19 : 8; proto : -1 : string;
      Bits.to_bin extended_bits : -1 : string;
      SHA1.to_bin ih : 20 * 8 : string;
      SHA1.to_bin id : 20 * 8 : string }

let read_handshake hs sock =
  (* sock#write_bitstring (handshake_message hs.id hs.skey) >>= fun () -> *)
  sock#read_string (49 + String.length proto) >>= fun str ->
  bitmatch Bitstring.bitstring_of_string str with
  | { 19 : 8;
      proto : 19 * 8 : string;
      extbits : 8 * 8 : string, bind (Bits.of_bin extbits);
      ih : 20 * 8 : string, bind (SHA1.from_bin ih);
      id : 20 * 8 : string, bind (SHA1.from_bin id) } ->
    if SHA1.equal hs.skey ih then
      Lwt.return (Success (id, extbits, sock))
    else
      fail_lwt "info-hash mismatch received:%s expected:%s" (SHA1.to_hex ih) (SHA1.to_hex hs.skey)

let read_handshake_or_ya hs sock =
  assert false

let send_handshake hs sock =
  let m = handshake_message hs.id hs.skey in
  let m = Bitstring.string_of_bitstring m in
  sock#write_int16 (String.length m) >>= fun () ->
  sock#write_string m >>= fun () ->
  Log.info "wrote handshake:%S" m;
  Lwt.return ()
    
(* let unsafe_incoming ~id sock = *)
(* read_handshake sock >>= fun (ih1, id1, extbits) -> *)
(* Tcp.write_bitstring sock (handshake_message id ih1) >>= fun () -> *)
(* Lwt.return (`Plain (ih1, id1, extbits)) *)

(*
4 B->A: ENCRYPT(VC, crypto_select, len(padD), padD), ENCRYPT2(Payload Stream)
*)
let read_vc hs secret sock =
  let buf = String.create (String.length vc) in
  let buf1 = String.create (String.length vc) in
  let rec loop p =
    if p >= max_pad_len then
      fail_lwt "could not find vc"
    else begin
      let c = arcfour ~incoming:true ~secret ~skey:(SHA1.to_bin hs.skey) in
      c#transform buf 0 buf1 0 (String.length vc);
      if buf1 = vc then
        Lwt.return c
      else begin
        String.blit buf 1 buf 0 (String.length vc - 1);
        sock#really_read buf (String.length vc - 1) 1 >>= fun () ->
        loop (p+1)
      end
    end
  in
  Lwt.try_bind
    begin fun () ->
      sock#really_read buf 0 (String.length vc) >>= fun () ->
      loop 0 >>= fun dec ->
      Log.info "got vc";
      Lwt.return dec
    end
    begin fun dec ->
      sock#enable_decryption dec;
      sock#read_int32 >>= fun crypto_select ->
      Log.info "got crypto_select: %lx" crypto_select;
      sock#read_int16 >>= fun pad_d_len ->
      Log.info "got pad_d_len:%d" pad_d_len;
      sock#read_string pad_d_len >>= fun _ ->
      match crypto_select with
      | 0x02l ->
        Log.info "encrypted handshake";
        read_handshake hs (sock :> IO.socket)
      | 0x01l ->
        Log.info "plain handshake";
        sock#disable_encryption;
        sock#disable_decryption;
        read_handshake hs (sock :> IO.socket)
      | _ ->
        fail_lwt "unknown crypto_select: %lx" crypto_select
    end
    begin fun _ -> Lwt.return Retry end

(*
2 B->A: Diffie Hellman Yb, PadB
3 A->B: HASH('req1', S), HASH('req2', SKEY) xor HASH('req3', S), ENCRYPT(VC, crypto_provide, len(PadC), PadC, len(IA)), ENCRYPT(IA)
*)
let read_yb hs sock =
  let doit () =
    sock#read_string key_len >>= fun yb ->
    Log.info "received yb";
    let secret = Cryptokit.DH.shared_secret dh_params hs.private_key yb in
    (* HASH ('req1', S) *)
    sock#write_string (hash ["req1"; secret]) >>= fun () ->
    (* HASH ('req2', SKEY) xor HASH ('req3', S) *)
    let req2 = hash ["req2"; SHA1.to_bin hs.skey] in
    let req3 = hash ["req3"; secret] in
    Cryptokit.xor_string req2 0 req3 0 (String.length req2);
    sock#write_string req3 >>= fun () ->
    Lwt.return secret
  in
  Lwt.try_bind doit
    begin fun secret ->
      (* ENCRYPT (VC, crypto_provide, len (PadC), PadC
         PadC is reserved for future extensions to the handshake...
         standard practice at this time is for it to be zero-length *)
      let arc4encrypt = arcfour ~incoming:false (* hs.incoming *) ~secret ~skey:(SHA1.to_bin hs.skey) in
      let sock = IO.encrypt sock in
      sock#enable_encryption arc4encrypt;
      sock#write_string vc >>= fun () ->
      Log.info "wrote vc";
      sock#write_int32 crypto_provide >>= fun () ->
      Log.info "wrote crypto_provide:%lX" crypto_provide;
      sock#write_int16 0 >>= fun () ->
      (* ENCRYPT len (IA)), ENCRYPT (IA) *)
      send_handshake hs sock >>= fun () ->
      read_vc hs secret sock
    end
    begin fun _ -> Lwt.return Retry end

(*
1 A->B: Diffie Hellman Ya, PadA
*)
let send_ya hs sock =
  let ya = Cryptokit.DH.message dh_params hs.private_key in
  sock#write_string ya >>= fun () ->
  let pad_len = Random.int (max_pad_len+1) in
  let pad = Cryptokit.Random.string Cryptokit.Random.secure_rng pad_len in
  sock#write_string pad >>= fun () ->
  read_yb hs sock

(* let read_crypto_select hs ic oc = *)
(*   read_int32 fd ic >>= fun n -> *)
(*   Log.info "crypto select is %ld" n; *)
(*   read_int16 fd >>= fun pad_d_len -> *)
(*   assert (0 <= pad_d_len && pad_d_len <= 512); *)
(*   read_pad_d hs ic oc *)

(* let read_pad_d hs ic oc pad_d_len = *)
(*   read_string ic pad_d_len >>= fun _ -> *)
(*   read_handshake hs ic oc *)

(* let read_ya hs ic oc = *)
(*   let ya = String.create key_len in *)
(*   read_string fd key_len >>= fun ya -> *)
(*   let secret = Cryptokit.DH.shared_secret dh_params hs.private_key ya in *)
(*   (\* let key = sha1 ["req1"; secret; hs.skey] in *\) *)
(*   Log.info "sending B->A: Diffie Hellman Yb, PadB"; *)
(*   let public_key = Cryptokit.DH.message dh_params hs.private_key in *)
(*   write_string fd public_key >>= fun () -> *)
(*   (\* padding *\) *)
(*   read_pad_a hs ic oc *)

let run hs encrypted sock =
  if hs.incoming then
    read_handshake_or_ya hs sock
  else if encrypted then
    send_ya hs sock
  else
    send_handshake hs sock >>= fun () -> read_handshake hs sock
