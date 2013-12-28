open Info

let failwith fmt =
  Printf.ksprintf failwith fmt

let max_pipelined_requests = 5
let lo_mark = 5
let hi_mark = 25
let default_block_size = 16 * 1024
let keepalive_delay = 20 (* FIXME *)
let request_backlog = 5
let rate_update_frequency = 3
  
type piece_progress = {
  mutable piecenum : int;
  mutable bytesgot : int;
  mutable lastoffset : int;
  buffer : string
}

type download_status =
  | IDLE
  | ACTIVE of piece_progress

type t = {
  sa : Lwt_unix.sockaddr;
  id : Word160.t;
  torrent : Torrent.t;
  mutable am_interested : bool;
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable peer_choking : bool;
  have : Bits.t;
  mutable status : download_status;
  mutable active_requests : (int * int * int) list;
  stop : unit -> unit;
  write : [ `Literal of string | `Block of int * int * int ] -> unit;
  ul : float Lwt_react.S.t;
  dl : float Lwt_react.S.t;
  update_dl : int -> unit;
  update_ul : int -> unit;
  fastext : bool;
  extensions : (string, int) Hashtbl.t
  (* mutable ext_handshake : (string * Bcode.t) list *)
}

let string_of_sockaddr = function
  | Lwt_unix.ADDR_UNIX s ->
    s
  | Lwt_unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

let to_string self =
  Printf.sprintf "%s@%s [%c%c|%c%c|%d/%d]"
    (Word160.to_hex_short self.id)
    (string_of_sockaddr self.sa)
    (if self.am_choking then 'C' else 'c')
    (if self.am_interested then 'I' else 'i')
    (if self.peer_choking then 'C' else 'c')
    (if self.peer_interested then 'I' else 'i')
    (Bits.count self.have) (Bits.length self.have)

let stop self =
  Trace.infof "STOPPING %s" (to_string self);
  self.stop ()

let send_message self message =
  let s = Wire.put message |> Put.run in
  `Literal s |> self.write

let send_block self i off len =
  `Block (i, off, len) |> self.write
    
let got_ut_metadata self m =
  assert false
  (* let m = Bcode.from_string m in *)
  (* let msg_type = Bcode.find "msg_type" m |> Bcode.to_int in *)
  (* let piece = Bcode.find "piece" m |> Bcode.to_int in *)
  (* match msg_type with *)
  (* | 0 (\* request *\) -> *)
  (*   let id = Hashtbl.find "ut_metadata" pr.extensions in *)
  (*   if (\* has complete metadata *\) then *)
  (*     let d = *)
  (*       ["msg_type", Bcode.BInt 1L; *)
  (*        "piece", Bcode.BInt (Int64.of_int piece); *)
  (*        "total_size", (\* size of metadata *\)] *)
  (*     in *)
  (*     let msg = Bcode.bencode (Bcode.BDict d) @ (\* metadata piece *\) in *)
  (*     send pr (SendMsg Wire.Extended (id, msg)) *)
  (*   else *)
  (*     let d = *)
  (*       ["msg_type", Bcode.BInt 2L; *)
  (*        "piece", Bcode.BInt (Int64.of_int piece)] *)
  (*     in *)
  (*     send pr (SendMsg Wire.Extended (id, Bcode.bencode (Bcode.BDict d))) *)
  (* | 1 (\* data *\) -> *)
  (*   (\* add data to metainfo *\) *)
  (* | 2 (\* reject *\) -> *)
  (*   (\* fine, ignore *\) *)
  (* | _ -> *)
  (*   (\* An unrecognised msg_type must be ignored, see *)
  (*      http://www.bittorrent.org/beps/bep_0009.html *\) *)
  (*   Trace.infof "Unknown ut_metadata msg_type %d from %s, ignoring" *)
  (*     msg_type (to_string pr) *)

let supported_extensions = [
  1, ("ut_metadata", got_ut_metadata)
]

let send_extended_hanshake self =
  let m =
    List.map (fun (id, (name, _)) ->
        name, Bcode.BInt (Int64.of_int id)) supported_extensions
  in
  let m = Bcode.BDict ["m", Bcode.BDict m] in
  let m = Bcode.bencode m |> Put.run in
  send_message self (Wire.EXTENDED (0, m))

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

(* let available_pieces pr = *)
(*   Bits.copy pr.peer_pieces *)

let got_choke self =
  if not self.peer_choking then begin
    Trace.infof "%s choked us" (to_string self);
    self.peer_choking <- true;
    match self.status with
    | IDLE ->
      ()
    | ACTIVE c ->
      Torrent.request_lost self.torrent c.piecenum;
      self.status <- IDLE
  end

(* let _piecelen self piece = *)
(*   if piece < Array.length self.info.hashes-1 then *)
(*     self.info.piece_size *)
(*   else *)
(*     self.info.total_length - piece*self.info.piece_size *)

let want self i =
  Bits.is_set self.have i

let request_more self =
  (* assert (not self.peer_choking); *)
  match self.status with
  | ACTIVE c ->
    while List.length self.active_requests < request_backlog &&
          c.lastoffset < String.length c.buffer do
      let len = min (String.length c.buffer - c.lastoffset) default_block_size in
      self.active_requests <- (c.piecenum, c.lastoffset, len) :: self.active_requests;
      send_message self (Wire.REQUEST (c.piecenum, c.lastoffset, len));
      c.lastoffset <- c.lastoffset + len
    done
  | IDLE ->
    assert false

let ready self =
  match self.status with
  | IDLE when not self.peer_choking ->
    begin
      match Torrent.next_piece self.torrent (want self) with
      | None ->
        self.status <- IDLE
      | Some (piecenum, piecelen) ->
        let p =
          { piecenum; lastoffset = 0; bytesgot = 0; buffer = String.create piecelen }
        in
        self.status <- ACTIVE p;
        request_more self
    end
  | IDLE
  | ACTIVE _ -> ()
                
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
  if i < 0 || i >= Bits.length self.have then
    failwith "%s has invalid #%d" (to_string self) i;
  Bits.set self.have i;
  Trace.infof "%s has obtained #%d and now has %d/%d pieces"
    (to_string self) i (Bits.count self.have) (Bits.length self.have);
  Torrent.got_have self.torrent i

let got_have_bitfield self b =
  (* if self.got_anything then failwith "bitfield must come first"; *)
  (* if Bits.count t.peer_pieces = 0 then begin *)
  (* FIXME padding, should only come after handshake *)
  Bits.blit b 0 self.have 0 (Bits.length self.have);
  Bits.iteri (fun i b -> if b then Torrent.got_have self.torrent i) b

let got_request self i off len =
  if self.am_choking then
    failwith "%s violating protocol, terminating exchange" (to_string self);
  (* FIXME *)
  send_block self i off len

let got_piece self i off s =
  match self.status with
  | IDLE ->
    failwith "%s sent us a piece but we are not not downloading, terminating"
      (to_string self)
  | ACTIVE c ->
    if c.piecenum <> i then
      failwith "%s sent some blocks for unrequested piece, terminating" (to_string self);
    (* FIXME check that the length is ok *)
    self.active_requests <-
      List.filter (fun (i1, off1, _) -> not (i1 = i && off1 = off)) self.active_requests;
    String.blit s 0 c.buffer off (String.length s);
    c.bytesgot <- c.bytesgot + String.length s;
    if c.bytesgot = String.length c.buffer then begin
      ignore (Torrent.got_piece self.torrent i c.buffer);
      self.status <- IDLE;
      ready self
    end else
      request_more self

let got_have_all self =
  for i = 0 to Bits.length self.have do
    Bits.set self.have i;
    Torrent.got_have self.torrent i
  done

let got_extended_handshake self m =
  let m = Get.run Bcode.bdecode m |> Bcode.find "m" |> Bcode.to_dict in
  List.iter (fun (name, id) ->
      let id = Bcode.to_int id in
      if id = 0 then Hashtbl.remove self.extensions name
      else Hashtbl.replace self.extensions name id) m;
  Trace.infof "%s supports EXTENDED: %s" (to_string self)
    (String.concat " " (List.map (fun (name, _) -> name) m))

let got_extended self id m =
  if List.mem_assoc id supported_extensions then
    let _, f = List.assoc id supported_extensions in
    f self m
  else      
    Trace.infof "%s sent unsupported EXTENDED message %d" (to_string self) id

let got_message self message =
  Trace.recv (to_string self) (Wire.string_of_message message);
  match message with
  | Wire.KEEP_ALIVE -> ()
  | Wire.CHOKE -> got_choke self
  | Wire.UNCHOKE -> got_unchoke self
  | Wire.INTERESTED -> got_interested self
  | Wire.NOT_INTERESTED -> got_not_interested self
  | Wire.HAVE i -> got_have self i
  | Wire.BITFIELD _ -> stop self (* got_bitfield self b *)
  | Wire.REQUEST (i, off, len) -> got_request self i off len
  | Wire.PIECE (i, off, s) -> got_piece self i off s
  | Wire.CANCEL (i, off, len) -> ()
  | Wire.HAVE_ALL
  | Wire.HAVE_NONE -> stop self
  | Wire.EXTENDED (0, m) -> got_extended_handshake self m
  | Wire.EXTENDED (id, m) -> got_extended self id m
  | _ ->
    Trace.infof "Unhandled message from %s: %s" (to_string self) (Wire.string_of_message message);
    stop self

let got_first_message self message =
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
  Bits.iteri (fun i b -> if b then Torrent.lost_have self.torrent i) self.have;
  (* self.disconnected (); *)
  match self.status with
  | IDLE -> ()
  | ACTIVE c -> Torrent.request_lost self.torrent c.piecenum

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

let writer_loop oc get_block write_queue stop_thread stop =
  let keepalive_message = Wire.put Wire.KEEP_ALIVE |> Put.run in
  let rec loop () =
    Lwt.pick
      [ Lwt_stream.next write_queue >|= (fun x -> `Write x);
        stop_thread >|= (fun () -> `Stop);
        Lwt_unix.sleep (float keepalive_delay) >|= (fun () -> `Timeout) ] >>= function
    | `Write (`Literal s) ->
      Lwt_io.write oc s >>= loop
    | `Write (`Block (i, off, len)) ->
      get_block i off len >>= begin function
        | None ->
          Lwt.wrap stop
        | Some s ->
          Wire.PIECE (i, off, s) |> Wire.put |> Put.run |> Lwt_io.write oc >>= loop
      end
    | `Stop ->
      Lwt.return_unit
    | `Timeout ->
      Lwt_io.write oc keepalive_message >>= loop
  in
  Lwt.catch loop
    (fun exn ->
       Trace.infof ~exn "Error in writer loop";
       Lwt.wrap stop) >>= fun () -> Lwt_io.close oc

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

let fastextbit = 6*8+3

let create sa id ic oc (minfo : Info.t) exts =
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
      am_interested = false;
      peer_interested = false;
      am_choking = true;
      peer_choking = true;
      have = Bits.create (Array.length minfo.pieces);
      active_requests = [];
      status = IDLE;
      torrent = Torrent.create minfo;
      stop;
      write = (fun s -> write (Some s));
      ul; dl;
      update_dl; update_ul;
      fastext = Bits.is_set exts fastextbit;
      extensions = Hashtbl.create 17 }
  in
  ticker_loop resetdl resetul stop_thread |> ignore;
  reader_loop ic (got_first_message self) (got_message self) stop_thread stop |> ignore;
  writer_loop oc (Torrent.get_block self.torrent) write_queue stop_thread stop |> ignore;
  let bits = Torrent.completed self.torrent in
  if Bits.count bits > 0 then send_message self (Wire.BITFIELD bits);
  self

let ul pr =
  Lwt_react.S.value pr.ul

let dl pr =
  Lwt_react.S.value pr.dl
