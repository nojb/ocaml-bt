open Info

let failwith fmt =
  Printf.ksprintf failwith fmt

let kilobytes n = n * 1024

let max_pipelined_requests = 5
let lo_mark = 5
let hi_mark = 25
let default_block_size = 16 * 1024
let keepalive_delay = 20 (* FIXME *)
let request_backlog = 5
let rate_update_frequency = 3
let info_piece_size = kilobytes 16
  
type piece_progress = {
  piecenum : int;
  mutable bytesgot : int;
  mutable lastoffset : int;
  buffer : string
}

type download_status = {
  torrent : Torrent.t;
  mutable activity : [`IDLE | `ACTIVE of piece_progress];
  have : Bits.t;
  active_requests : (int * int * int) Hashset.t;
  info : Info.t;
  dl_write : [`LIT of string | `BLOCK of int * int * int] -> unit
}

type info_status = {
  info_hash : Word160.t;
  mutable length : [`NOSIZE | `GOTSIZE of int * string array];
  on_completion : Info.t -> unit;
  info_write : string -> unit
}

type peer_status =
  | DOWNLOAD of download_status
  | INFO of info_status

type t = {
  sa : Lwt_unix.sockaddr;
  id : Word160.t;
  status : peer_status;
  mutable am_interested : bool;
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable peer_choking : bool;
  stop : unit -> unit;
  (* write : [ `Literal of string | `Block of int * int * int ] -> unit; *)
  ul : float Lwt_react.S.t;
  dl : float Lwt_react.S.t;
  update_dl : int -> unit;
  update_ul : int -> unit;
  fastext : bool;
  ltext : bool;
  extensions : (string, int) Hashtbl.t
  (* mutable ext_handshake : (string * Bcode.t) list *)
}

let string_of_sockaddr = function
  | Lwt_unix.ADDR_UNIX s ->
    s
  | Lwt_unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

let sprint self () =
  Printf.sprintf "%s [%c%c|%c%c%s]"
    (Word160.to_hex_short self.id)
    (* (string_of_sockaddr self.sa) *)
    (if self.am_choking then 'C' else 'c')
    (if self.am_interested then 'I' else 'i')
    (if self.peer_choking then 'C' else 'c')
    (if self.peer_interested then 'I' else 'i')
    (match self.status with
     | DOWNLOAD dl -> Printf.sprintf "|%d/%d" (Bits.count dl.have) (Bits.length dl.have)
     | INFO _ -> "")

let to_string self =
  sprint self ()

let stop self =
  Trace.infof "STOPPING %s" (to_string self);
  self.stop ()

let send_message self message =
  let s = Wire.put message |> Put.run in
  Trace.sent (to_string self) (Wire.string_of_message message);
  match self.status with
  | INFO info ->
    s |> info.info_write
  | DOWNLOAD dl ->
    `LIT s |> dl.dl_write

let send_block self i off len =
  match self.status with
  | INFO _ ->
    assert false
  | DOWNLOAD dl ->
    `BLOCK (i, off, len) |> dl.dl_write

let request_info_piece self i =
  let id = Hashtbl.find self.extensions "ut_metadata" in
  let d =
    [ "msg_type", Bcode.BInt 0L;
      "piece", Bcode.BInt (Int64.of_int i) ]
  in
  let s = Bcode.bencode (Bcode.BDict d) |> Put.run in
  Wire.EXTENDED (id, s) |> send_message self;
  Trace.infof "%s: requested piece %d" (to_string self) i

let request_more self =
  (* assert (not self.peer_choking); *)
  match self.status with
  | DOWNLOAD dl ->
    begin match dl.activity with
      | `ACTIVE c ->
        while Hashset.cardinal dl.active_requests < request_backlog &&
              c.lastoffset < String.length c.buffer do
          let len = min (String.length c.buffer - c.lastoffset) default_block_size in
          Hashset.add dl.active_requests (c.piecenum, c.lastoffset, len);
          send_message self (Wire.REQUEST (c.piecenum, c.lastoffset, len));
          c.lastoffset <- c.lastoffset + len
        done
      | `IDLE ->
        assert false
    end
  | INFO nfo ->
    assert false

let got_ut_metadata self s =
  let m, data_start = Get.run_partial Bcode.bdecode s in
  let msg_type = Bcode.find "msg_type" m |> Bcode.to_int in
  let piece = Bcode.find "piece" m |> Bcode.to_int in
  let id = Hashtbl.find self.extensions "ut_metadata" in
  match msg_type, self.status with
  | 0, INFO _ (* request *) ->
    let d =
      [ "msg_type", Bcode.BInt 2L;
        "piece", Bcode.BInt (Int64.of_int piece) ]
    in
    let s = Bcode.bencode (Bcode.BDict d) |> Put.run in
    Wire.EXTENDED (id, s) |> send_message self
  | 0, DOWNLOAD dl ->
    let d =
      [ "msg_type", Bcode.BInt 2L;
        "piece", Bcode.BInt (Int64.of_int piece);
        "total_size", Bcode.BInt (Int64.of_int (Info.length dl.info)) ]
    in
    let s =
      Info.get_piece dl.info piece |> Put.string |>
      Put.bind (Bcode.bencode (Bcode.BDict d)) |> Put.run
    in
    Wire.EXTENDED (id, s) |> send_message self
  | 1, INFO nfo (* data *) ->
    let p = String.sub s data_start (String.length s - data_start) in
    begin match nfo.length with
      | `GOTSIZE (_, pieces) ->
        Trace.infof "%s: got info piece %d" (to_string self) piece;
        pieces.(piece) <- p;
        if piece+1 >= Array.length pieces then begin
          Trace.infof "%s: all info pieces received!" (to_string self);
          let s = Array.to_list pieces |> String.concat "" in
          if Word160.equal (Word160.digest_of_string s) nfo.info_hash then begin
            (* Trace.infof "%s: info data validated: %S" (to_string self) s; *)
            let bc = Get.run Bcode.bdecode s in
            nfo.on_completion (create bc)
          end else begin
            Trace.infof "%s: info data invalid, stopping" (to_string self);
            stop self
          end
        end else
          request_info_piece self (piece+1)
      | `NOSIZE ->
        assert false
    end
  | 1, DOWNLOAD _ ->
    assert false (* FIXME - stop ? *)
  | 2, INFO nfo (* reject *) ->
    Trace.infof "%s: rejected request for info piece %d, stopping"
      (to_string self) piece;
    stop self
  | 2, DOWNLOAD _ ->
    assert false (* FIXME - stop ? *)
  | _ ->
    (* An unrecognised msg_type must be ignored, see
       http://www.bittorrent.org/beps/bep_0009.html, 'extension message' *)
    Trace.infof "Unknown ut_metadata msg_type %d from %s, ignoring"
      msg_type (to_string self)

let supported_extensions = [
  1, ("ut_metadata", got_ut_metadata)
]

let send_extended_handshake self =
  let m =
    List.map (fun (id, (name, _)) ->
        name, Bcode.BInt (Int64.of_int id)) supported_extensions
  in
  let m = Bcode.BDict ["m", Bcode.BDict m] in
  let m = Bcode.bencode m |> Put.run in
  Wire.EXTENDED (0, m) |> send_message self

(* let request_more_blocks peer = *)
(*   match peer.current with *)
(*   | None -> *)
(*     () *)
(*   | Some c -> *)
(*     (\* debug "requesting more blocks from %s for piece #%d" *\) *)
(*       (\* (to_string pr) c.index; *\) *)
(*     let plen = peer.pieces.(c.index).Info.piece_length in *)
(*     while List.length peer.active_requests < max_pipelined_requests && *)
(*       c.last_offset < plen do *)
(*       let blen = min default_block_size (plen-c.last_offset) in *)
(*       c.last_offset <- c.last_offset + blen; *)
(*       peer.active_requests <- (c.index, c.last_offset, blen) :: peer.active_requests; *)
(*       send_message peer (Wire.REQUEST (c.index, c.last_offset, blen)) *)
(*     done *)

(* let download_piece pr index = *)
(*   match pr.current with *)
(*   | Some _ -> *)
(*     failwith "download_piece: peer already downloading piece; what is going on?" *)
(*   | None -> *)
(*     Trace.infof "Requesting #%d from %s" index (to_string pr); *)
(*     pr.current <- Some *)
(*         { index; received = 0; last_offset = 0; *)
(*           buffer = String.create (pr.pieces.(index).Info.piece_length) }; *)
(*     request_more_blocks pr *)

let got_choke self =
  if not self.peer_choking then begin
    Trace.infof "%s choked us" (to_string self);
    self.peer_choking <- true;
    match self.status with
    | DOWNLOAD dl ->
      begin match dl.activity with
      | `IDLE -> ()
      | `ACTIVE c ->
        Torrent.request_lost dl.torrent c.piecenum;
        dl.activity <- `IDLE
      end
    | INFO _ ->
      ()
  end
      
let ready self =
  match self.status with
  | DOWNLOAD dl ->
    begin match dl.activity with
      | `IDLE when not self.peer_choking ->
        begin
          match Torrent.next_piece dl.torrent (Bits.is_set dl.have) with
          | None ->
            dl.activity <- `IDLE
          | Some (piecenum, piecelen) ->
            let p =
              { piecenum; lastoffset = 0; bytesgot = 0; buffer = String.create piecelen }
            in
            dl.activity <- `ACTIVE p;
            request_more self
        end
      | `IDLE
      | `ACTIVE _ -> ()
    end
  | INFO _ ->
    ()
      
let got_unchoke self =
  if self.peer_choking then begin
    Trace.infof "%s unchoked us" (to_string self);
    self.peer_choking <- false;
    if self.am_interested then ready self
  end
  
let got_interested self =
  if not self.peer_interested then begin
    Trace.infof "%s is interested in us" (to_string self);
    self.peer_interested <- true
  end

let got_not_interested self =
  if self.peer_interested then begin
    Trace.infof "%s is no longer interested in us" (to_string self);
    self.peer_interested <- false;
  end

let got_have self i =
  match self.status with
  | DOWNLOAD dl ->
    if i < 0 || i >= Bits.length dl.have then
      failwith "%s has invalid #%d" (to_string self) i;
    Trace.infof "%s has obtained #%d and now has %d/%d pieces"
      (to_string self) i (Bits.count dl.have) (Bits.length dl.have);
    Bits.set dl.have i;
    Torrent.got_have dl.torrent i
  | INFO _ ->
    ()

let got_have_bitfield self b =
  match self.status with
  | DOWNLOAD dl ->
    Bits.blit b 0 dl.have 0 (Bits.length dl.have);
    Bits.iteri (fun i b -> if b then Torrent.got_have dl.torrent i) b
  | INFO _ ->
    ()

let got_request self i off len =
  if self.am_choking then
    failwith "%s violating protocol, terminating exchange" (to_string self);
  (* FIXME *)
  match self.status with
  | DOWNLOAD _ ->
    send_block self i off len
  | INFO _ ->
    ()

let got_piece self i off s =
  match self.status with
  | INFO _ ->
    failwith "peer sent us a piece inf INFO mode!?!?!"
  | DOWNLOAD dl ->
    match dl.activity with
    | `IDLE ->
      failwith "%s sent us a piece but we are not not downloading, terminating"
        (to_string self)
    | `ACTIVE c ->
      if c.piecenum <> i then
        failwith "%s sent some blocks for unrequested piece, terminating" (to_string self);
      (* FIXME check that the length is ok *)
      Hashset.remove dl.active_requests (i, off, String.length s);
      String.blit s 0 c.buffer off (String.length s);
      c.bytesgot <- c.bytesgot + String.length s;
      if c.bytesgot = String.length c.buffer then begin
        ignore (Torrent.got_piece dl.torrent i c.buffer);
        dl.activity <- `IDLE;
        ready self
      end else
        request_more self

let got_have_all self =
  match self.status with
  | INFO _ ->
    ()
  | DOWNLOAD dl ->
    for i = 0 to Bits.length dl.have do
      Bits.set dl.have i;
      Torrent.got_have dl.torrent i
    done

let roundup n r =
  (n + r - 1) / r * r

let got_extended_handshake self m =
  let bc = Get.run Bcode.bdecode m in
  let m =
    Bcode.find "m" bc |> Bcode.to_dict |>
    List.map (fun (name, id) -> (name, Bcode.to_int id))
  in
  List.iter (fun (name, id) ->
      (* let id = Bcode.to_int id in *)
      if id = 0 then Hashtbl.remove self.extensions name
      else Hashtbl.replace self.extensions name id) m;
  Trace.infof "%s: EXTENDED handshake: %s" (to_string self)
    (List.map (fun (name, id) ->
         Printf.sprintf "%s (%d)" name id) m |> String.concat ", ");
  match self.status with
  | INFO nfo ->
    if List.exists (fun (name, _) -> name = "ut_metadata") m then begin
      let l = Bcode.find "metadata_size" bc |> Bcode.to_int in
      let numpieces = roundup l info_piece_size / info_piece_size in
      let last_piece_size = l mod info_piece_size in
      Trace.infof "%t: got metadata size: %d, %d pieces, last piece: %d"
        (sprint self) l numpieces last_piece_size;
      let pieces = Array.init numpieces (fun i ->
          if i < numpieces - 1 then String.make info_piece_size '\000'
          else String.make last_piece_size '\000') in
      nfo.length <- `GOTSIZE (l, pieces);
      request_info_piece self 0
    end else
      let _ = Trace.infof "%t does not support ut_metadata, stopping" (sprint self) in
      stop self
  | DOWNLOAD _ ->
    ()

let got_extended self id m =
  if List.mem_assoc id supported_extensions then
    let _, f = List.assoc id supported_extensions in
    f self m
  else      
    Trace.infof "%t: unsupported EXTENDED message %d" (sprint self) id

let got_message self message =
  Trace.recv (to_string self) (Wire.string_of_message message);
  match message with
  | Wire.KEEP_ALIVE -> ()
  | Wire.CHOKE -> got_choke self
  | Wire.UNCHOKE -> got_unchoke self
  | Wire.INTERESTED -> got_interested self
  | Wire.NOT_INTERESTED -> got_not_interested self
  | Wire.HAVE i -> got_have self i
  | Wire.BITFIELD bits ->
    if self.ltext then got_have_bitfield self bits else stop self
  | Wire.REQUEST (i, off, len) -> got_request self i off len
  | Wire.PIECE (i, off, s) -> got_piece self i off s
  | Wire.CANCEL (i, off, len) -> ()
  | Wire.HAVE_ALL
  | Wire.HAVE_NONE -> stop self
  | Wire.EXTENDED (0, m) -> got_extended_handshake self m
  | Wire.EXTENDED (id, m) -> got_extended self id m
  | _ ->
    Trace.infof "Unhandled message from %t: %t" (sprint self) (Wire.sprint message);
    stop self

let got_first_message self message =
  Trace.recv (to_string self) (Wire.string_of_message message);
  match message with
  | Wire.BITFIELD bits -> got_have_bitfield self bits
  | Wire.HAVE_ALL -> if self.fastext then got_have_all self else stop self
  | Wire.HAVE_NONE -> if self.fastext then () else stop self
  | _ -> if self.fastext then stop self else got_message self message

let piece_completed self i =
    send_message self (Wire.HAVE i)

let is_interested self =
  self.peer_interested

let is_choked self =
  self.am_choking

(* let interesting self = *)
(*   if not self.interesting then begin *)
(*     self.we_interested <- true; *)
(*     send_message self Wire.INTERESTED *)
(*   end *)

(* let not_interesting self = *)
(*   if self.we_interested then begin *)
(*     self.we_interested <- false; *)
(*     send_message self Wire.NOT_INTERESTED *)
(*   end *)

let choke self =
  if not self.am_choking then begin
    self.am_choking <- true;
    send_message self Wire.CHOKE
  end

let unchoke self =
  if self.am_choking then begin
    self.am_choking <- false;
    send_message self Wire.UNCHOKE
  end

let disconnected self =
  (* self.disconnected (); *)
  match self.status with
  | INFO _ ->
    ()
  | DOWNLOAD dl ->
    Bits.iteri (fun i b -> if b then Torrent.lost_have dl.torrent i) dl.have;
    match dl.activity with
    | `IDLE -> ()
    | `ACTIVE c -> Torrent.request_lost dl.torrent c.piecenum

let make_rate r =
  let reset, updatereset = Lwt_react.E.create () in
  let total () = Lwt_react.S.fold (+) 0 r in
  let average () =
    let start = Unix.gettimeofday () in
    Lwt_react.S.map (fun x -> (float x) /. (Unix.gettimeofday () -. start)) (total ())
  in
  let rt = Lwt_react.S.switch (average ()) (Lwt_react.E.map average reset) in
  rt, updatereset

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let writer_loop_partial oc write_queue stop_thread stop =
  let keepalive_message = Wire.put Wire.KEEP_ALIVE |> Put.run in
  let rec loop () =
    Lwt.pick
      [ Lwt_stream.next write_queue >|= (fun x -> `Write x);
        stop_thread >|= (fun () -> `Stop);
        Lwt_unix.sleep (float keepalive_delay) >|= (fun () -> `Timeout) ] >>= function
    | `Write s ->
      Lwt_io.write oc s >>= loop
    | `Stop ->
      Lwt.return_unit
    | `Timeout ->
      Lwt_io.write oc keepalive_message >>= loop
  in
  Lwt.catch loop
    (fun exn ->
       Trace.infof ~exn "Error in writer loop";
       Lwt.wrap stop) >>= fun () -> Lwt_io.close oc
    
(* let writer_loop oc get_block write_queue stop_thread stop = *)
(*   let keepalive_message = Wire.put Wire.KEEP_ALIVE |> Put.run in *)
(*   let rec loop () = *)
(*     Lwt.pick *)
(*       [ Lwt_stream.next write_queue >|= (fun x -> `Write x); *)
(*         stop_thread >|= (fun () -> `Stop); *)
(*         Lwt_unix.sleep (float keepalive_delay) >|= (fun () -> `Timeout) ] >>= function *)
(*     | `Write (`Literal s) -> *)
(*       Lwt_io.write oc s >>= loop *)
(*     | `Write (`Block (i, off, len)) -> *)
(*       get_block i off len >>= begin function *)
(*         | None -> *)
(*           Lwt.wrap stop *)
(*         | Some s -> *)
(*           Wire.PIECE (i, off, s) |> Wire.put |> Put.run |> Lwt_io.write oc >>= loop *)
(*       end *)
(*     | `Stop -> *)
(*       Lwt.return_unit *)
(*     | `Timeout -> *)
(*       Lwt_io.write oc keepalive_message >>= loop *)
(*   in *)
(*   Lwt.catch loop *)
(*     (fun exn -> *)
(*        Trace.infof ~exn "Error in writer loop"; *)
(*        Lwt.wrap stop) >>= fun () -> Lwt_io.close oc *)

let reader_loop ic got_first_message got_message stop_thread stop =
  let input = Lwt_stream.from (fun () -> Wire.read ic >|= fun msg -> Some msg) in
  let rec loop gotmsg =
    Lwt.pick
      [ Lwt_stream.next input >|= (fun x -> `Ok x);
        stop_thread >|= (fun () -> `Stop);
        Lwt_unix.sleep (float keepalive_delay) >|= (fun () -> `Timeout) ] >>= function
    | `Ok x ->
      gotmsg x;
      loop got_message
    | `Stop ->
      Lwt.return_unit
    | `Timeout ->
      Lwt.wrap stop
  in
  Lwt.catch
    (fun () -> loop got_first_message)
    (fun exn ->
       Trace.infof ~exn "Error in reader loop";
       Lwt.wrap stop) >>= fun () -> Lwt_io.close ic

let ticker_loop resetdl resetul stop_thread =
  let rec loop () =
    Lwt.pick [Lwt_unix.sleep (float rate_update_frequency) >|= (fun () -> `Update);
              stop_thread >|= (fun () -> `Stop)] >>= function
    | `Update ->
      resetdl ();
      resetul ();
      loop ()
    | `Stop ->
      Lwt.return_unit
  in
  loop ()

let fastextbit = 63 - (6*8+3)
let ltextbit = 63 - 20

let partial_exts =
  let b = Bits.create (8 * 8) in
  Bits.set b ltextbit;
  b

let create_with_partial sa id ic oc info_hash on_completion exts =
  let write_queue, write = Lwt_stream.create () in
  let dl, update_dl = Lwt_react.E.create () in
  let ul, update_ul = Lwt_react.E.create () in
  let dl, resetdl = make_rate dl in
  let ul, resetul = make_rate ul in
  let stop_thread, wake_stop = Lwt.wait () in
  let stop () = Lwt.wakeup wake_stop () in
  let self =
    { sa;
      id;
      status = INFO { info_hash; length = `NOSIZE; on_completion;
                      info_write = (fun x -> write (Some x)) };
      am_interested = false;
      peer_interested = false;
      am_choking = true;
      peer_choking = true;
      (* have = Bits.create (Array.length minfo.pieces); *)
      (* active_requests = []; *)
      (* status = IDLE; *)
      (* torrent = Torrent.create minfo; *)
      stop;
      (* write = (fun s -> write (Some s)); *)
      ul; dl;
      update_dl; update_ul;
      fastext = Bits.is_set exts fastextbit;
      ltext = Bits.is_set exts ltextbit;
      extensions = Hashtbl.create 17 }
  in
  if Bits.is_set exts ltextbit then
    Trace.infof "%s supports EXTENDED" (to_string self);
  if Bits.is_set exts fastextbit then
    Trace.infof "%s supports FAST" (to_string self);
  ticker_loop resetdl resetul stop_thread |> ignore;
  reader_loop ic (got_first_message self) (got_message self) stop_thread stop |> ignore;
  writer_loop_partial oc write_queue stop_thread stop |> ignore;
  send_extended_handshake self;
  (* writer_loop oc (Torrent.get_block self.torrent) write_queue stop_thread stop |> ignore; *)
  (* let bits = Torrent.completed self.torrent in *)
  (* if Bits.count bits > 0 then send_message self (Wire.BITFIELD bits); *)
  (* request_more self; *)
  self

let ul pr =
  Lwt_react.S.value pr.ul

let dl pr =
  Lwt_react.S.value pr.dl
