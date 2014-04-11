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

(* let failwith fmt = *)
(*   Printf.ksprintf failwith fmt *)

let (>>=) = Lwt.(>>=)

(* let log ?exn fmt = *)
(*   Printf.ksprintf (fun msg -> Lwt_log.ign_debug_f ?exn "[peer] %s" msg) fmt *)

let error ?exn fmt = Log.error ~sec:"peer" ?exn fmt
let warning ?exn fmt = Log.warning ~sec:"peer" ?exn fmt
let info ?exn fmt = Log.info ~sec:"peer" ?exn fmt
let success ?exn fmt = Log.success ~sec:"peer" ?exn fmt

type event =
  [ `Choked of (int * int * int) list
  | `Unchoked
  | `Interested
  | `NotInterested
  | `Have of int
  | `Request of int * int * int
  | `Piece of int * int * string
  | `MetaPiece of int * string
  | `BitField of Bits.t
  | `Port of int
  | `Finished
  | `AvailableMetadata of int ]

let kilobyte n = n * 1024
(* let lo_mark = 5 *)
(* let hi_mark = 25 *)
let keepalive_delay = 20 (* FIXME *)
(* let request_backlog = 5 *)
(* let rate_update_frequency = 3 *)
let info_piece_size = kilobyte 16
let default_block_size = kilobyte 16

let extract_assoc key seq =
  let n = Lwt_sequence.find_node_r (fun (key', _) -> key = key') seq in
  Lwt_sequence.remove n;
  snd (Lwt_sequence.get n)

exception Timeout

type send_item =
  [ `Msg of Wire.message
  | `Block of int * int * int ]

type request_item =
  [ `Block of int * int * int
  | `Info of int ]

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
  can_request : unit Lwt_condition.t;
  requests : (request_item * string Lwt.u) Lwt_sequence.t;
  send_queue : send_item Lwt_sequence.t;
  can_send : unit Lwt_condition.t;
  mutable handle : event -> unit;
  mutable info : Info.t option;
  mutable have : Bits.t
}

(* let flush p = *)
(*   Lwt_condition.broadcast p.can_send () *)

let send_message p m =
  ignore (Lwt_sequence.add_r (`Msg m) p.send_queue)
  (* flush p *)

let send_extended p id s =
  send_message p (Wire.EXTENDED (id, s))

let send_block p i o l =
  ignore (Lwt_sequence.add_r (`Block (i, o, l)) p.send_queue)
  (* flush p *)

let got_ut_metadata p data =
  let m, data_start = Get.run_partial Bcode.bdecode data in
  let msg_type = Bcode.to_int (Bcode.find "msg_type" m) in
  let piece = Bcode.to_int (Bcode.find "piece" m) in
  let data = String.sub data data_start (String.length data - data_start) in
  let id = Hashtbl.find p.extensions "ut_metadata" in
  match msg_type with
  | 0 -> (* request *)
    let m =
      let open Put in
      match p.info with
      | None ->
        let d =
          [ "msg_type", Bcode.BInt 2L;
            "piece", Bcode.BInt (Int64.of_int piece) ] in
        Bcode.bencode (Bcode.BDict d)
      | Some info ->
        let d =
          [ "msg_type", Bcode.BInt 1L;
            "piece", Bcode.BInt (Int64.of_int piece);
            "piece_size", Bcode.BInt (Int64.of_int (Info.length info)) ] in
        Bcode.bencode (Bcode.BDict d) >> string (Info.get_piece info piece)
    in
    send_extended p id (Put.run m)
  | 1 -> (* data *)
    let wake = extract_assoc (`Info piece) p.requests in
    Lwt_condition.broadcast p.can_request ();
    Lwt.wakeup wake data
  | 2 -> (* reject *)
    let wake = extract_assoc (`Info piece) p.requests in
    Lwt.wakeup_exn wake Not_found
  | _ ->
    ()

let supported_extensions =
  [ 1, ("ut_metadata", got_ut_metadata) ]

let got_choke p =
  if not p.peer_choking then begin
    p.peer_choking <- true;
    p.handle (`Choked []) (* XXX *)
  end

let got_unchoke p =
  if p.peer_choking then begin
    p.peer_choking <- false;
    p.handle `Unchoked
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
  begin match p.info with
  | None ->
    p.have <- b
  | Some info ->
    Bits.blit b 0 p.have 0 (Array.length info.Info.hashes)
  end;
  for i = 0 to Bits.length b - 1 do
    if Bits.is_set b i then p.handle (`Have i)
  done

let got_have p idx =
  begin match p.info with
  | None ->
    if idx >= Bits.length p.have then begin
      let bits = Bits.create (2 * (Bits.length p.have)) in
      Bits.blit p.have 0 bits 0 (Bits.length p.have);
      p.have <- bits
    end;
    Bits.set p.have idx
  | Some _ ->
    Bits.set p.have idx
  end;
  p.handle (`Have idx)

let got_cancel p idx off len =
  Lwt_sequence.iter_node_l
    (fun n ->
       match Lwt_sequence.get n with
       | `Block (idx', off', len') ->
         if idx = idx' && off = off' && len = len' then
           Lwt_sequence.remove n
       | `Msg _ -> ()) p.send_queue

let got_piece p idx off s =
  try
    let wake = extract_assoc (`Block (idx, off, String.length s)) p.requests in
    Lwt_condition.broadcast p.can_request ();
    Lwt.wakeup wake s
  with
  | Not_found ->
    failwith "unrequested block"

let got_extended_handshake p bc =
  let m =
    Bcode.find "m" bc |> Bcode.to_dict |>
    List.map (fun (name, id) -> (name, Bcode.to_int id))
  in
  List.iter (fun (name, id) ->
      if id = 0 then Hashtbl.remove p.extensions name
      else Hashtbl.replace p.extensions name id) m;
  if Hashtbl.mem p.extensions "ut_metadata" then
    p.handle (`AvailableMetadata (Bcode.find "metadata_size" bc |> Bcode.to_int))

let got_extended p id data =
  let (_, f) = List.assoc id supported_extensions in
  f p data

let got_request p idx off len =
  ()

let got_message p m =
  info "received message from %s: %s" (Addr.to_string p.addr) (Wire.string_of_message m);
  (* Trace.recv self#sprint (Wire.sprint m); *)
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
      (fun () -> Wire.read p.sock >>= fun msg -> Lwt.return (Some msg))
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

let get_block p idx off len =
  assert false

let writer_loop p =
  let keepalive_msg = Put.run (Wire.put Wire.KEEP_ALIVE) in
  let rec loop () =
    Lwt.pick [
      (Lwt_unix.sleep (float keepalive_delay) >>= fun () -> Lwt.return `Timeout);
      (p.should_stop >>= fun () -> Lwt.return `Stop);
      (Lwt.pause () >>= fun () -> Lwt.return `Ready)
      (* (Lwt_condition.wait p.can_send >>= fun () -> Lwt.return `Ready) *)
    ]
    >>= function
    | `Timeout ->
      Tcp.write p.sock keepalive_msg >>= loop
    | `Stop ->
      Lwt.return_unit
    | `Ready ->
      let rec loop' () =
        match Lwt_sequence.take_opt_l p.send_queue with
        | None ->
          loop ()
        | Some (`Msg m) ->
          Wire.put m |> Put.run |> Tcp.write p.sock >>= fun () ->
          info "sent message to %s: %s" (Addr.to_string p.addr) (Wire.string_of_message m);
          loop' ()
        | Some (`Block (idx, off, len)) ->
          get_block p idx off len >>= fun s ->
          let m = Wire.PIECE (idx, off, s) in
          Wire.put m |> Put.run |> Tcp.write p.sock >>= fun () ->
          loop' ()
      in
      loop' ()
  in
  loop ()

let create sock id =
  let w, wake = Lwt.wait () in
  let p =
    { addr = Tcp.getpeeraddr sock; sock; id;
      am_choking = true; am_interested = false;
      peer_choking = true; peer_interested = false;
      should_stop = w; extbits = Bits.create (8 * 8);
      extensions = Hashtbl.create 17;
      can_request = Lwt_condition.create ();
      requests = Lwt_sequence.create ();
      send_queue = Lwt_sequence.create ();
      can_send = Lwt_condition.create ();
      info = None;
      handle = (fun _ -> ());
      have = Bits.create 0 }
  in
  p

let start p handle =
  p.handle <- handle;
  let run_loop () =
    Lwt.catch
      (fun () -> Lwt.join [reader_loop p; writer_loop p])
      (fun e ->
         error ~exn:e "peer input/output error (addr=%s,id=%s)"
           (Addr.to_string p.addr) (Word160.to_hex_short p.id);
         Lwt.return ())
    >>= fun () ->
    handle `Finished;
    Lwt.return ()
  in
  run_loop () |> ignore

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

let has_piece p ~idx =
  Bits.is_set p.have idx

let have p =
  Bits.copy p.have

let send_choke p =
  if not p.am_choking then begin
    p.am_choking <- true;
    send_message p Wire.CHOKE
  end

let send_unchoke p =
  if p.am_choking then begin
    p.am_choking <- false;
    send_message p Wire.UNCHOKE
  end

let send_interested p =
  if not p.am_interested then begin
    p.am_interested <- true;
    send_message p Wire.INTERESTED
  end

let send_not_interested p =
  if p.am_interested then begin
    p.am_interested <- false;
    send_message p Wire.NOT_INTERESTED
  end

let send_have p ~idx =
  send_message p (Wire.HAVE idx)

let request_count seq =
  let n = ref 0 in
  Lwt_sequence.iter_l (function
      | `Block _, _ -> incr n
      | `Info _, _ -> ()) seq;
  !n

let info_request_count seq =
  let n = ref 0 in
  Lwt_sequence.iter_l (function
      | `Block _, _ -> ()
      | `Info _, _ -> incr n) seq;
  !n

let pipeline_number = 5

let request_block p idx off len =
  assert (idx >= 0 && off >= 0 && len >= 0);
  let w, wake = Lwt.wait () in
  ignore (Lwt_sequence.add_r (`Block (idx, off, len), wake) p.requests);
  send_message p (Wire.REQUEST (idx, off, len));
  w

let request_piece p ?block_size:(blk_size=default_block_size) idx len =
  assert (idx >= 0 && len >= 0 && blk_size >= 0);
  let s = String.create len in
  let rec loop ws off =
    if off >= len then
      Lwt.join ws
    else
    if request_count p.requests < pipeline_number then
      let sz = min blk_size (len - off) in
      let w =
        request_block p idx off sz >>= fun buf ->
        String.blit buf 0 s off sz;
        Lwt.return ()
      in
      loop (w :: ws) (off + sz)
    else
      Lwt_condition.wait p.can_request >>= fun () -> loop ws off
  in
  loop [] 0 >>= fun () ->
  Lwt.return s

let request_meta_piece p idx =
  assert (idx >= 0);
  assert (Hashtbl.mem p.extensions "ut_metadata");
  let id = Hashtbl.find p.extensions "ut_metadata" in
  let d =
    [ "msg_type", Bcode.BInt 0L;
      "piece", Bcode.BInt (Int64.of_int idx) ]
  in
  let w, wake = Lwt.wait () in
  ignore (Lwt_sequence.add_l (`Info idx, wake) p.requests);
  Bcode.bencode (Bcode.BDict d) |> Put.run |> send_extended p id;
  w

(* let request_meta_piece p idx len = *)
(*   let metadata_size = match p.metadata_size with *)
(*     | None -> failwith "no metadata_size" *)
(*     | Some n -> n *)
(*   in *)
(*   let s = String.create metadata_size in *)
(*   let rec loop ws idx = *)
(*     if idx * info_piece_size >= metadata_size then *)
(*       Lwt.join ws *)
(*     else *)
(*     if info_request_count p.requests < pipeline_number then *)
(*       let w = *)
(*         request_info_piece p idx >>= fun buf -> *)
(*         String.blit buf 0 s (idx * info_piece_size) (String.length buf); *)
(*         Lwt.return () *)
(*       in *)
(*       loop (w :: ws) (idx + 1) *)
(*     else *)
(*       Lwt_condition.wait p.can_request >>= fun () -> loop ws idx *)
(*   in *)
(*   loop [] 0 >>= fun () -> *)
(*   Lwt.return s *)

(* let fastextbit = 63 - (6*8+3) *)

(* class virtual peer fd (id, exts) on_death = *)
(*     val extensions : (string, int) Hashtbl.t = Hashtbl.create 17 *)
(*     val fastext = Bits.is_set exts fastextbit *)
(*     val ltext = Bits.is_set exts ltextbit *)

    (* method sprint () = *)
    (*   Printf.sprintf "%s [%c%c|%c%c]" *)
    (*     (Word160.to_hex_short id) *)
    (*     (\* (string_of_sockaddr self.sa) *\) *)
    (*     (if am_choking then 'C' else 'c') *)
    (*     (if am_interested then 'I' else 'i') *)
    (*     (if peer_choking then 'C' else 'c') *)
    (*     (if peer_interested then 'I' else 'i') *)

    (* method stop = *)
    (*   match Lwt.state stop_thread with *)
    (*   | Lwt.Return () -> *)
    (*     Trace.infof "%t: already stopped" self#sprint *)
    (*   | _ -> *)
    (*     Trace.infof "%t: stopping..." self#sprint; *)
    (*     Lwt.wakeup wake_stop () *)

(* let roundup n r = *)
(*   (n + r - 1) / r * r *)

(* class info_peer fd info_hash (id, exts) on_death = *)
(*   object (self) *)
    (* val mutable length : string array option = None *)

    (* method private request_info_piece i = *)
    (*   let id = Hashtbl.find extensions "ut_metadata" in *)
    (*   let d = *)
    (*     [ "msg_type", Bcode.BInt 0L; *)
    (*       "piece", Bcode.BInt (Int64.of_int i) ] *)
    (*   in *)
    (*   Bcode.bencode (Bcode.BDict d) |> Put.run |> self#send_extended id; *)
    (*   Trace.infof "%t: requested piece %d" self#sprint i *)
  
    (* method got_ut_metadata msg_type piece s = *)
    (*   let id = Hashtbl.find extensions "ut_metadata" in *)
    (*   match msg_type, length with *)
    (*   | 0, _ -> (\* request *\) *)
    (*     let d = *)
    (*       [ "msg_type", Bcode.BInt 2L; *)
    (*         "piece", Bcode.BInt (Int64.of_int piece) ] *)
    (*     in *)
    (*     Bcode.bencode (Bcode.BDict d) |> Put.run |> self#send_extended id *)
    (*   | 1, None -> (\* data *\) *)
    (*     raise (UnexpectedMetadataMessage piece) *)
    (*     (\* self#stop *\) *)
    (*   | 1, Some pieces -> *)
    (*     Trace.infof "%t: got info piece %d" self#sprint piece; *)
    (*     pieces.(piece) <- s; *)
    (*     if piece+1 >= Array.length pieces then begin *)
    (*       Trace.infof "%t: all info pieces received!" self#sprint; *)
    (*       let all = Array.to_list pieces |> String.concat "" in *)
    (*       if Word160.equal (Word160.digest_of_string all) info_hash then begin *)
    (*         (\* Trace.infof "%s: info data validated: %S" (to_string self) s; *\) *)
    (*         Get.run Bcode.bdecode all |> Info.create |> on_completion; *)
    (*         self#stop (\* FIXME raise Exit *\) *)
    (*       end else *)
    (*         raise BadInfoHash *)
    (*         (\* Trace.infof "%t: info data invalid, stopping" self#sprint; *\) *)
    (*       (\*   self#stop *\) *)
    (*       (\* end *\) *)
    (*     end else *)
    (*       self#request_info_piece (piece+1) *)
    (*   | 2, _ -> (\* reject *\) *)
    (*     raise (InfoRequestRejected piece) *)
    (*     (\* Trace.infof "%t: rejected request for info piece %d, stopping" *\) *)
    (*     (\*   self#sprint piece; *\) *)
    (*     (\* self#stop *\) *)
    (*   | _ -> *)
    (*     (\* An unrecognised msg_type must be ignored, see *)
    (*        http://www.bittorrent.org/beps/bep_0009.html, 'extension message' *\) *)
    (*     Trace.infof "%t: unknown ut_metadata msg_type %d, ignoring" *)
    (*       self#sprint msg_type *)

    (* method got_extended_handshake bc = *)
    (*   Trace.infof "%s: info_peer: got_extended_handshake" (Word160.to_hex_short id); *)
    (*   super#got_extended_handshake bc; *)
    (*   if Hashtbl.mem extensions "ut_metadata" then begin *)
    (*     let l = Bcode.find "metadata_size" bc |> Bcode.to_int in *)
    (*     let numpieces = roundup l info_piece_size / info_piece_size in *)
    (*     let last_piece_size = l mod info_piece_size in *)
    (*     Trace.infof "%t: got metadata size: %d, %d pieces, last piece: %d" *)
    (*       self#sprint l numpieces last_piece_size; *)
    (*     let pieces = Array.init numpieces (fun i -> *)
    (*         if i < numpieces - 1 then String.make info_piece_size '\000' *)
    (*         else String.make last_piece_size '\000') in *)
    (*     length <- Some pieces; *)
    (*     self#request_info_piece 0 *)
    (*   end else *)
    (*     raise NoMetadataSupport *)
      (*   begin *)
      (*   Trace.infof "%t does not support ut_metadata, stopping" self#sprint; *)
      (*   self#stop *)
      (* end *)

  (* end *)

(* exception InvalidBlockRequest of int * int * int *)

(* module S2 = Set.Make (struct type t = (int * int) let compare = compare end) *)

(* class sharing fd info_hash info torrent (id, exts) on_death = *)
(*   object (self) *)
(*     val have = Bits.create (Array.length info.Info.hashes) *)
(*     val mutable stage : sharing_stage option = None *)

(*     inherit peer fd (id, exts) on_death as super *)

(*     method get_block i o l = *)
(*       Lwt.catch *)
(*         (fun () -> Torrent.get_block torrent i o l) *)
(*         (fun _ -> Lwt.fail (InvalidBlockRequest (i, o, l))) *)
(*       (\* Torrent.get_block torrent i o l >|= function *\) *)
(*       (\* | None -> raise (InvalidBlockRequest (i, o, l)) *\) *)
(*       (\* | Some s -> s *\) *)

(*     method sprint () = *)
(*       Printf.sprintf "%t [%d/%d]" super#sprint (Bits.count have) (Bits.length have) *)

    (* method got_ut_metadata msg_type piece s = *)
    (*   let id = Hashtbl.find extensions "ut_metadata" in *)
    (*   match msg_type with *)
    (*   | 0 -> (\* request *\) *)
    (*     let d = *)
    (*       [ "msg_type", Bcode.BInt 2L; *)
    (*         "piece", Bcode.BInt (Int64.of_int piece); *)
    (*         "total_size", Bcode.BInt (Int64.of_int (Info.length info)) ] *)
    (*     in *)
    (*     let s = *)
    (*       Info.get_piece info piece |> Put.string |> *)
    (*       Put.bind (Bcode.bencode (Bcode.BDict d)) |> Put.run *)
    (*     in *)
    (*     Wire.EXTENDED (id, s) |> self#send_message *)
    (*   | 1 *)
    (*   | 2 -> *)
    (*     raise (UnexpectedMetadataMessage piece) *)
    (*   (\* raise UnrequestedInfoData piece *\) *)
    (*   (\* failwith "%t: got_ut_metadata: bad msg_type: %d" self#sprint msg_type *\) *)
    (*   | _ -> *)
    (*     () *)

    (* method got_have_all = *)
    (*   for i = 0 to Bits.length have - 1 do *)
    (*     Bits.set have i; *)
    (*     Torrent.got_have torrent i *)
    (*   done *)

    (* method got_have i = *)
    (*   if not (Bits.is_set have i) then begin *)
    (*     Bits.set have i; *)
    (*     Torrent.got_have torrent i *)
    (*   end *)

    (* method got_choke = *)
    (*   super#got_choke; *)
    (*   match stage with *)
    (*   | Some pc -> *)
    (*     stage <- None; *)
    (*     Torrent.request_lost torrent pc.pc_index *)
    (*   | None -> *)
    (*     () *)
      (* S3.iter (fun (i, o, l) -> Picker.request_lost picker i o l) active_requests *)

    (* method want i = *)
    (*   Bits.is_set have i && Torrent.available_requests torrent *)

  (*   method got_request i o l = *)
  (*     if not am_choking || peer_interested then *)
  (*       self#send_block i o l *)
  (*     else if fastext then *)
  (*       self#send_message (Wire.REJECT (i, o, l)) *)

  (*   method private send_my_handshake = *)
  (*     if Torrent.numgot torrent > 0 then *)
  (*       self#send_message (Wire.BITFIELD (Torrent.have torrent)) *)

(* type t = { *)
(*   sa : Lwt_unix.sockaddr; *)
(*   id : Word160.t; *)
(*   status : peer_status; *)
(*   stop : unit -> unit; *)
(*   (\* write : [ `Literal of string | `Block of int * int * int ] -> unit; *\) *)
(*   ul : float Lwt_react.S.t; *)
(*   dl : float Lwt_react.S.t; *)
(*   update_dl : int -> unit; *)
(*   update_ul : int -> unit; *)
(*   fastext : bool; *)
(*   ltext : bool; *)
(* } *)

(* let stop self = *)
(*   Trace.infof "STOPPING %s" (to_string self); *)
(*   self.stop () *)

(* let disconnected self = *)
(*   (\* self.disconnected (); *\) *)
(*   match self.status with *)
(*   | INFO _ -> *)
(*     () *)
(*   | DOWNLOAD dl -> *)
(*     Bits.iteri (fun i b -> if b then Picker.lost_have dl.picker i) dl.have; *)
(*     Hashset.iter (fun (i, off, len) -> *)
(*         Torrent.request_lost dl.torrent i off len) dl.active_requests *)

(* let make_rate r = *)
(*   let reset, updatereset = Lwt_react.E.create () in *)
(*   let total () = Lwt_react.S.fold (+) 0 r in *)
(*   let average () = *)
(*     let start = Unix.gettimeofday () in *)
(*     Lwt_react.S.map (fun x -> (float x) /. (Unix.gettimeofday () -. start)) (total ()) *)
(*   in *)
(*   let rt = Lwt_react.S.switch (average ()) (Lwt_react.E.map average reset) in *)
(*   rt, updatereset *)

(* let ticker_loop resetdl resetul stop_thread = *)
(*   let rec loop () = *)
(*     Lwt.pick [Lwt_unix.sleep (float rate_update_frequency) >|= (fun () -> `Update); *)
(*               stop_thread >|= (fun () -> `Stop)] >>= function *)
(*     | `Update -> *)
(*       resetdl (); *)
(*       resetul (); *)
(*       loop () *)
(*     | `Stop -> *)
(*       Lwt.return_unit *)
(*   in *)
(*   loop () *)

(* let fastextbit = 63 - (6*8+3) *)
(* let ltextbit = 63 - 20 *)

(* let create_with_partial sa id ic oc info_hash on_completion exts = *)
(*   let write_queue, write = Lwt_stream.create () in *)
(*   let dl, update_dl = Lwt_react.E.create () in *)
(*   let ul, update_ul = Lwt_react.E.create () in *)
(*   let dl, resetdl = make_rate dl in *)
(*   let ul, resetul = make_rate ul in *)
(*   let stop_thread, wake_stop = Lwt.wait () in *)
(*   let stop () = Lwt.wakeup wake_stop () in *)
(*   let self = *)
(*     { sa; *)
(*       id; *)
(*       status = INFO { info_hash; length = `NOSIZE; on_completion; *)
(*                       info_write = (fun x -> write (Some x)) }; *)
(*       am_interested = false; *)
(*       peer_interested = false; *)
(*       am_choking = true; *)
(*       peer_choking = true; *)
(*       (\* have = Bits.create (Array.length minfo.pieces); *\) *)
(*       (\* active_requests = []; *\) *)
(*       (\* status = IDLE; *\) *)
(*       (\* torrent = Torrent.create minfo; *\) *)
(*       stop; *)
(*       (\* write = (fun s -> write (Some s)); *\) *)
(*       ul; dl; *)
(*       update_dl; update_ul; *)
(*       fastext = Bits.is_set exts fastextbit; *)
(*       ltext = Bits.is_set exts ltextbit; *)
(*       extensions = Hashtbl.create 17 } *)
(*   in *)
(*   if Bits.is_set exts ltextbit then *)
(*     Trace.infof "%s supports EXTENDED" (to_string self); *)
(*   if Bits.is_set exts fastextbit then *)
(*     Trace.infof "%s supports FAST" (to_string self); *)
(*   ticker_loop resetdl resetul stop_thread |> ignore; *)
(*   reader_loop ic (got_first_message self) (got_message self) stop_thread stop |> ignore; *)
(*   writer_loop_partial oc write_queue stop_thread stop |> ignore; *)
(*   send_extended_handshake self; *)
(*   (\* writer_loop oc (Torrent.get_block self.torrent) write_queue stop_thread stop |> ignore; *\) *)
(*   (\* let bits = Torrent.completed self.torrent in *\) *)
(*   (\* if Bits.count bits > 0 then send_message self (Wire.BITFIELD bits); *\) *)
(*   (\* request_more self; *\) *)
(*   self *)

(* let ul pr = *)
(*   Lwt_react.S.value pr.ul *)

(* let dl pr = *)
(*   Lwt_react.S.value pr.dl  *)

let set_metadata p info =
  p.info <- Some info

let wait_ready p =
  (* Lwt_condition.wait p.ready *)
  assert false
