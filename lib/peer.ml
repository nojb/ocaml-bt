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

type event =
  | Choked of (int * int * int) list
  | Unchoked
  | Interested
  | NotInterested
  | Have of int
  | HaveBitfield of Bits.t
  | BlockRequested of int * int * int
  | BlockReceived of int * int * string
  | Port of int
  | Finished of (int * int * int) list
  | AvailableMetadata of int
  | MetaRequested of int
  | GotMetaPiece of int * string
  | RejectMetaPiece of int

let kilobyte n = n * 1024
(* let lo_mark = 5 *)
(* let hi_mark = 25 *)
let keepalive_delay = 20 (* FIXME *)
(* let request_backlog = 5 *)
(* let rate_update_frequency = 3 *)
let request_pipeline_max = 5
let info_piece_size = kilobyte 16
let default_block_size = kilobyte 16

exception Timeout

type t = {
  addr : Addr.t;
  sock : Tcp.socket;
  mutable id : Word160.t;
  mutable am_choking : bool;
  mutable am_interested : bool;
  mutable peer_choking : bool;
  mutable peer_interested : bool;
  should_stop : unit Lwt.t;
  (* stop : unit Lwt.t; *)
  extbits : Bits.t;
  extensions : (string, int) Hashtbl.t;
  
  requests : (int * int * int) Lwt_stream.t;
  mutable act_reqs : (int * int * int) list;
  send_req : (int * int * int) -> unit;
  
  msg_queue : Wire.message Lwt_stream.t;
  send : Wire.message -> unit;
  
  mutable handle : (event -> unit) option;
  mutable have : Bits.t;

  mutable upload : Rate.t;
  mutable download : Rate.t
}

let signal p e =
  match p.handle with
  | None -> ()
  | Some h -> h e
                
let send_extended p id s =
  p.send (Wire.EXTENDED (id, s))

let send_block p i o s =
  p.send (Wire.PIECE (i, o, s))

let got_ut_metadata p data =
  let m, data_start = Get.run_partial Bcode.bdecode data in
  let msg_type = Bcode.to_int (Bcode.find "msg_type" m) in
  let piece = Bcode.to_int (Bcode.find "piece" m) in
  let data = String.sub data data_start (String.length data - data_start) in
  match msg_type with
  | 0 -> (* request *)
    signal p (MetaRequested piece)
  | 1 -> (* data *)
    signal p (GotMetaPiece (piece, data))
  | 2 -> (* reject *)
    signal p (RejectMetaPiece piece)
  | _ ->
    ()

let send_reject_meta p piece =
  let id = Hashtbl.find p.extensions "ut_metadata" in
  let m =
    let open Put in
    let d = [ "msg_type", Bcode.BInt 2L; "piece", Bcode.BInt (Int64.of_int piece) ] in
    Bcode.bencode (Bcode.BDict d)
  in
  send_extended p id (Put.run m)

let send_meta_piece p piece (len, s) =
  let id = Hashtbl.find p.extensions "ut_metadata" in
  let m =
    let open Put in
    let d =
      [ "msg_type", Bcode.BInt 1L;
        "piece", Bcode.BInt (Int64.of_int piece);
        "total_size", Bcode.BInt (Int64.of_int len) ] in
    Bcode.bencode (Bcode.BDict d) >> string s
  in
  send_extended p id (Put.run m)
  
let supported_extensions =
  [ 1, ("ut_metadata", got_ut_metadata) ]

let got_choke p =
  if not p.peer_choking then begin
    p.peer_choking <- true;
    signal p (Choked p.act_reqs)
  end

let got_unchoke p =
  if p.peer_choking then begin
    p.peer_choking <- false;
    signal p Unchoked
  end

let got_interested p =
  if not p.peer_interested then begin
    p.peer_interested <- true
  end

let got_not_interested p =
  if p.peer_interested then begin
    p.peer_interested <- false
  end

let got_have_bitfield p b =
  p.have <- b;
  signal p (HaveBitfield b)

let got_have p idx =
  if idx >= Bits.length p.have then begin
    let bits = Bits.create (2 * (Bits.length p.have)) in
    Bits.blit p.have 0 bits 0 (Bits.length p.have);
    p.have <- bits
  end;
  Bits.set p.have idx;
  signal p (Have idx)

let got_cancel p _ _ _ =
  ()

let got_piece p idx off s =
  p.act_reqs <- List.filter (fun (i, o, l) -> (i, o, l) <> (idx, off, String.length s)) p.act_reqs;
  Rate.add p.download (String.length s);
  signal p (BlockReceived (idx, off, s))

let got_extended_handshake p bc =
  let m =
    Bcode.find "m" bc |> Bcode.to_dict |>
    List.map (fun (name, id) -> (name, Bcode.to_int id))
  in
  List.iter (fun (name, id) ->
      if id = 0 then Hashtbl.remove p.extensions name
      else Hashtbl.replace p.extensions name id) m;
  if Hashtbl.mem p.extensions "ut_metadata" then
    signal p (AvailableMetadata (Bcode.find "metadata_size" bc |> Bcode.to_int))

let got_extended p id data =
  let (_, f) = List.assoc id supported_extensions in
  f p data

let got_request p idx off len =
  signal p (BlockRequested (idx, off, len))

let got_message p m =
  match m with
  | Wire.KEEP_ALIVE -> ()
  | Wire.CHOKE -> got_choke p
  | Wire.UNCHOKE -> got_unchoke p
  | Wire.INTERESTED -> got_interested p
  | Wire.NOT_INTERESTED -> got_not_interested p
  | Wire.HAVE i -> got_have p i
  | Wire.BITFIELD b -> got_have_bitfield p b
  | Wire.REQUEST (idx, off, len) -> got_request p idx off len
  | Wire.PIECE (idx, off, s) -> got_piece p idx off s
  | Wire.CANCEL (idx, off, len) -> got_cancel p idx off len
  (* | Wire.HAVE_ALL *)
  (* | Wire.HAVE_NONE -> raise (InvalidProtocol m) *)
  | Wire.EXTENDED (0, s) -> got_extended_handshake p (Get.run Bcode.bdecode s)
  | Wire.EXTENDED (id, s) -> got_extended p id s
  | _ -> assert false

let reader_loop p =
  let input = Lwt_stream.from
      (fun () -> Wire.read p.sock >>= fun msg ->
        Log.debug "%s >>> %s" (Addr.to_string p.addr) (Wire.string_of_message msg);
        Lwt.return (Some msg))
  in
  let rec loop f =
    Lwt.pick [
      (Lwt_stream.next input >>= fun x -> Lwt.return (`Ok x));
      (p.should_stop >>= fun () -> Lwt.return `Stop);
      (Lwt_unix.sleep (float keepalive_delay) >>= fun () -> Lwt.return `Timeout)
    ]
    >>= function
    | `Ok x ->
      f x;
      loop f
    | `Stop ->
      Lwt.return ()
    | `Timeout ->
      Lwt.fail Timeout
  in
  loop (got_message p)

let send_request p (i, ofs, len) =
  p.send (Wire.REQUEST (i, ofs, len))

let writer_loop p =
  let keepalive_msg = Put.run (Wire.put Wire.KEEP_ALIVE) in
  let rec loop () =
    Lwt.pick [
      (Lwt_unix.sleep (float keepalive_delay) >>= fun () -> Lwt.return `Timeout);
      (p.should_stop >>= fun () -> Lwt.return `Stop);
      (Lwt_stream.next p.msg_queue >|= fun msg -> `Ready msg)
    ]
    >>= function
    | `Timeout ->
      Tcp.write p.sock keepalive_msg >>= loop
    | `Stop ->
      Lwt.return_unit
    | `Ready m ->
      Wire.put m |> Put.run |> Tcp.write p.sock >>= fun () ->
      Log.debug "%s <<< %s" (Addr.to_string p.addr) (Wire.string_of_message m);
      (match m with Wire.PIECE (_, _, s) -> Rate.add p.upload (String.length s) | _ -> ());
      loop ()
  in
  loop ()

let create sock id =
  let w, wake = Lwt.wait () in
  let requests, send_req = Lwt_stream.create () in
  let send_req x = send_req (Some x) in
  let msg_queue, send = Lwt_stream.create () in
  let send x = send (Some x) in
  let p =
    { addr = Tcp.getpeeraddr sock; sock; id;
      am_choking = true; am_interested = false;
      peer_choking = true; peer_interested = false;
      should_stop = w; extbits = Bits.create (8 * 8);
      extensions = Hashtbl.create 17;
      requests;
      act_reqs = [];
      send_req;
      msg_queue;
      send;
      handle = None;
      have = Bits.create 0;
      download = Rate.create ();
      upload = Rate.create () }
  in
  p

let start p h =
  p.handle <- Some h;
  let run_loop () =
    Lwt.catch
      (fun () -> Lwt.join [reader_loop p; writer_loop p])
      (fun e ->
         Log.error ~exn:e "peer input/output error (addr=%s,id=%s)"
           (Addr.to_string p.addr) (Word160.to_hex_short p.id);
         Lwt.return ())
    >>= fun () ->
    signal p (Finished p.act_reqs);
    Lwt.return ()
  in
  Lwt.async run_loop

let id p =
  p.id

let addr p =
  p.addr

let send_extended_handshake p =
  let m =
    List.map (fun (id, (name, _)) ->
        name, Bcode.BInt (Int64.of_int id)) supported_extensions
  in
  let m = Bcode.BDict ["m", Bcode.BDict m] in
  let s = Put.run (Bcode.bencode m) in
  send_extended p 0 s

let peer_choking p =
  p.peer_choking

let peer_interested p =
  p.peer_interested

let has_piece p idx =
  Bits.is_set p.have idx

let have p =
  Bits.copy p.have

let send_choke p =
  if not p.am_choking then begin
    p.am_choking <- true;
    p.send Wire.CHOKE
  end

let send_unchoke p =
  if p.am_choking then begin
    p.am_choking <- false;
    p.send Wire.UNCHOKE
  end

let send_interested p =
  if not p.am_interested then begin
    p.am_interested <- true;
    p.send Wire.INTERESTED
  end

let send_not_interested p =
  if p.am_interested then begin
    p.am_interested <- false;
    p.send Wire.NOT_INTERESTED
  end

let send_have p idx =
  p.send (Wire.HAVE idx)

let send_have_bitfield p bits =
  for i = 0 to Bits.length bits - 1 do
    if Bits.is_set bits i then p.send (Wire.HAVE i)
  done
  
(* let request_piece p ?block_size:(blk_size=default_block_size) idx len = *)
(*   assert (idx >= 0 && len >= 0 && blk_size >= 0); *)
(*   let s = String.create len in *)
(*   let rec loop ws off = *)
(*     if off >= len then *)
(*       Lwt.join ws *)
(*     else *)
(*       let sz = min blk_size (len - off) in *)
(*       let w = *)
(*         request_block p idx off sz >|= fun buf -> *)
(*         String.blit buf 0 s off sz *)
(*       in *)
(*       loop (w :: ws) (off + sz) *)
(*   in *)
(*   loop [] 0 >|= fun () -> s *)

let request_meta_piece p idx =
  assert (idx >= 0);
  assert (Hashtbl.mem p.extensions "ut_metadata");
  let id = Hashtbl.find p.extensions "ut_metadata" in
  let d =
    [ "msg_type", Bcode.BInt 0L;
      "piece", Bcode.BInt (Int64.of_int idx) ]
  in
  Bcode.bencode (Bcode.BDict d) |> Put.run |> send_extended p id
    
(* let rec request_meta_piece p idx = *)
(*   if info_request_count p.requests < pipeline_number then *)
(*     request_meta_piece0 p idx *)
(*   else *)
(*     Lwt_condition.wait p.can_request >>= fun () -> *)
(*     request_meta_piece p idx *)

(* let request_metadata p metadata_size = *)
(*   let s = String.create metadata_size in *)
(*   let rec loop ws idx = *)
(*     if idx * info_piece_size >= metadata_size then *)
(*       Lwt.join ws *)
(*     else *)
(*     if info_request_count p.requests < pipeline_number then *)
(*       let w = *)
(*         request_meta_piece p idx >>= fun buf -> *)
(*         String.blit buf 0 s (idx * info_piece_size) (String.length buf); *)
(*         Lwt.return () *)
(*       in *)
(*       loop (w :: ws) (idx + 1) *)
(*     else *)
(*       Lwt_condition.wait p.can_request >>= fun () -> loop ws idx *)
(*   in *)
(*   loop [] 0 >>= fun () -> *)
(*   Lwt.return s *)

let upload_rate p = Rate.get p.upload
let download_rate p = Rate.get p.download
