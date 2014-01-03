open Info

let failwith fmt =
  Printf.ksprintf failwith fmt

let kilobytes n = n * 1024

let max_pipelined_requests = 5
let lo_mark = 5
let hi_mark = 25
let keepalive_delay = 20 (* FIXME *)
let request_backlog = 5
let rate_update_frequency = 3
let info_piece_size = kilobytes 16

let supported_extensions =
  [ 1, "ut_metadata" ]

let reader_loop fd p stop_thread =
  let (>>=) = Lwt.(>>=) in
  let (>|=) = Lwt.(>|=) in
  let input = Lwt_stream.from (fun () -> Wire.read fd >|= fun msg -> Some msg) in
  let rec loop gotmsg =
    Lwt.pick
      [ Lwt_stream.next input >|= (fun x -> `Ok x);
        stop_thread >|= (fun () -> `Stop);
        Lwt_unix.sleep (float keepalive_delay) >|= (fun () -> `Timeout) ] >>= function
    | `Ok x ->
      gotmsg x;
      loop p#got_message
    | `Stop ->
      Lwt.return_unit
    | `Timeout ->
      p#stop;
      Lwt.return_unit
  in
  Lwt.catch
    (fun () -> loop p#got_first_message)
    (fun exn ->
       Trace.infof ~exn "%t: error in reader loop" p#sprint;
       p#stop;
       Lwt.return_unit)

let write_completely fd s =
  let (>>=) = Lwt.(>>=) in
  let rec loop o l =
    if l <= 0 then
      Lwt.return_unit
    else
      Lwt_unix.write fd s o l >>= fun l' ->
      loop (o+l') (l-l')
  in
  loop 0 (String.length s)

let writer_loop get_block error fd control q stop_thread =
  let (>>=) = Lwt.(>>=) in
  let (>|=) = Lwt.(>|=) in
  let keepalive_message = Wire.put Wire.KEEP_ALIVE |> Put.run in
  let rec loop () =
    Lwt.pick
      [ Lwt_unix.sleep (float keepalive_delay) >|= (fun () -> `Timeout);
        stop_thread >|= (fun () -> `Stop);
        Lwt_condition.wait control >|= (fun () -> `Ready) ] >>= function
    | `Timeout ->
      write_completely fd keepalive_message >>= loop
    | `Stop ->
      Lwt.return_unit
    | `Ready ->
      let rec loop' () =
        match Lwt_sequence.take_opt_l q with
        | None ->
          loop ()
        | Some (`RAW s) ->
          write_completely fd s >>= loop'
        | Some (`BLOCK (i, o, l)) ->
          get_block i o l >>= write_completely fd >>= loop'
      in
      loop' ()
  in
  Lwt.catch loop
    (fun exn ->
       error exn;
       Lwt.return_unit)

let fastextbit = 63 - (6*8+3)
let ltextbit = 63 - 20

class virtual peer fd (id, exts) =
  let stop_thread, wake_stop = Lwt.wait () in
  let control = Lwt_condition.create () in
  object (self)
    val mutable am_choking = true
    val mutable am_interested = false
    val mutable peer_choking = true
    val mutable peer_interested = false
    val extensions : (string, int) Hashtbl.t = Hashtbl.create 17
    val fastext = Bits.is_set exts fastextbit
    val ltext = Bits.is_set exts ltextbit
    val write_queue : [`RAW of string | `BLOCK of int * int * int] Lwt_sequence.t =
      Lwt_sequence.create ()

    method sprint () =
      Printf.sprintf "%s [%c%c|%c%c]"
        (Word160.to_hex_short id)
        (* (string_of_sockaddr self.sa) *)
        (if am_choking then 'C' else 'c')
        (if am_interested then 'I' else 'i')
        (if peer_choking then 'C' else 'c')
        (if peer_interested then 'I' else 'i')

    method virtual get_block : int -> int -> int -> string Lwt.t
    
    method flush =
      Lwt_condition.broadcast control ()

    method send_block i o l =
      Lwt_sequence.add_r (`BLOCK (i, o, l)) write_queue |> ignore;
      self#flush
    
    method private send_raw s =
      Lwt_sequence.add_r (`RAW s) write_queue |> ignore;
      self#flush

    method send_message m =
      Trace.sent self#sprint (Wire.sprint m);
      Wire.put m |> Put.run |> self#send_raw

    method got_choke =
      if not peer_choking then begin
        Trace.infof "%t choked us" self#sprint;
        peer_choking <- true
      end

    method got_unchoke =
      if peer_choking then begin
        Trace.infof "%t unchoked us" self#sprint;
        peer_choking <- false
      end

    method got_interested =
      if not peer_interested then begin
        Trace.infof "%t is interested in us" self#sprint;
        peer_interested <- true
      end

    method got_not_interested =
      if peer_interested then begin
        Trace.infof "%t is no longer interested in us" self#sprint;
        peer_interested <- false
      end

    method virtual got_have : int -> unit

    method virtual got_have_bitfield : Bits.t -> unit

    method virtual got_have_all : unit

    method virtual got_request : int -> int -> int -> unit

    method virtual got_piece : int -> int -> string -> unit

    method got_extended_handshake bc =
      let m =
        Bcode.find "m" bc |> Bcode.to_dict |>
        List.map (fun (name, id) -> (name, Bcode.to_int id))
      in
      List.iter (fun (name, id) ->
          if id = 0 then Hashtbl.remove extensions name
          else Hashtbl.replace extensions name id) m;
      Trace.infof "%t: EXTENDED handshake: %s"
        self#sprint
        (List.map (fun (name, id) ->
             Printf.sprintf "%s (%d)" name id) m |> String.concat ", ")

    method virtual got_ut_metadata : int -> int -> string -> unit

    method got_extended id s =
      if List.mem_assoc id supported_extensions then
        match List.assoc id supported_extensions with
        | "ut_metadata" ->
          let m, data_start = Get.run_partial Bcode.bdecode s in
          let msg_type = Bcode.find "msg_type" m |> Bcode.to_int in
          let piece = Bcode.find "piece" m |> Bcode.to_int in
          let l = String.length s in
          self#got_ut_metadata msg_type piece (String.sub s data_start (l-data_start))
        | s ->
          Trace.infof "%t: supported EXTENDED message %s is not_implemented"
            self#sprint s;
          assert false
      else
        self#stop
    (* Trace.infof "%t: unsupported EXTENDED message %d" self#sprint id *)

    method got_cancel i o l =
      Lwt_sequence.iter_node_l (fun n ->
          match Lwt_sequence.get n with
          | `BLOCK (i1, o1, l1) ->
            if i = i1 && o = o1 && l = l1 then
              Lwt_sequence.remove n
          | `RAW _ -> ()) write_queue

    method stop =
      match Lwt.state stop_thread with
      | Lwt.Return () ->
        Trace.infof "%t: already stopped" self#sprint
      | _ ->
        Trace.infof "%t: stopping..." self#sprint;
        Lwt.wakeup wake_stop ()

    method send_extended_handshake =
      let m =
        List.map (fun (id, name) ->
            name, Bcode.BInt (Int64.of_int id)) supported_extensions
      in
      let m = Bcode.BDict ["m", Bcode.BDict m] in
      let s = Bcode.bencode m |> Put.run in
      Wire.EXTENDED (0, s) |> self#send_message

    method send_extended id s =
      Wire.EXTENDED (id, s) |> self#send_message

    method got_message m =
      Trace.recv self#sprint (Wire.sprint m);
      match m with
      | Wire.KEEP_ALIVE -> ()
      | Wire.CHOKE -> self#got_choke
      | Wire.UNCHOKE -> self#got_unchoke
      | Wire.INTERESTED -> self#got_interested
      | Wire.NOT_INTERESTED -> self#got_not_interested
      | Wire.HAVE i -> self#got_have i
      | Wire.BITFIELD b -> self#got_have_bitfield b
      | Wire.REQUEST (i, o, l) -> self#got_request i o l
      | Wire.PIECE (i, o, s) -> self#got_piece i o s
      | Wire.CANCEL (i, o, l) -> self#got_cancel i o l
      | Wire.HAVE_ALL
      | Wire.HAVE_NONE -> self#stop
      | Wire.EXTENDED (0, s) -> self#got_extended_handshake (Get.run Bcode.bdecode s)
      | Wire.EXTENDED (id, s) -> self#got_extended id s
      | _ -> self#stop

    method got_first_message m =
      Trace.recv self#sprint (Wire.sprint m);
      match m with
      | Wire.BITFIELD b -> self#got_have_bitfield b
      | Wire.HAVE_ALL -> if fastext then self#got_have_all else self#stop
      | Wire.HAVE_NONE -> if fastext then () else self#stop
      | _ -> if fastext then self#stop else self#got_message m

    method choke =
      if not am_choking then begin
        am_choking <- true;
        self#send_message Wire.CHOKE
      end

    method unchoke =
      if am_choking then begin
        am_choking <- false;
        self#send_message Wire.UNCHOKE
      end

    method write_error exn =
      Trace.infof ~exn "%t: write error" self#sprint;
      self#stop

    initializer
      let event_loop () =
        Lwt.finalize (fun () ->
            Lwt.join
              [ reader_loop fd self stop_thread;
                writer_loop self#get_block self#write_error fd control write_queue stop_thread])
          (fun () -> Lwt_unix.close fd)
      in
      Lwt.async event_loop;
      self#send_extended_handshake
  end

let roundup n r =
  (n + r - 1) / r * r

let partial_exts =
  let b = Bits.create (8 * 8) in
  Bits.set b ltextbit;
  b

class info_peer fd info_hash on_completion (id, exts) =
  object (self)
    val mutable length : string array option = None
    val on_completion : Info.t -> unit = on_completion

    inherit peer fd (id, exts) as super

    method private request_info_piece i =
      let id = Hashtbl.find extensions "ut_metadata" in
      let d =
        [ "msg_type", Bcode.BInt 0L;
          "piece", Bcode.BInt (Int64.of_int i) ]
      in
      Bcode.bencode (Bcode.BDict d) |> Put.run |> self#send_extended id;
      Trace.infof "%t: requested piece %d" self#sprint i
  
    method got_ut_metadata msg_type piece s =
      let id = Hashtbl.find extensions "ut_metadata" in
      match msg_type, length with
      | 0, _ -> (* request *)
        let d =
          [ "msg_type", Bcode.BInt 2L;
            "piece", Bcode.BInt (Int64.of_int piece) ]
        in
        Bcode.bencode (Bcode.BDict d) |> Put.run |> self#send_extended id
      | 1, None -> (* data *)
        self#stop
      | 1, Some pieces ->
        Trace.infof "%t: got info piece %d" self#sprint piece;
        pieces.(piece) <- s;
        if piece+1 >= Array.length pieces then begin
          Trace.infof "%t: all info pieces received!" self#sprint;
          let all = Array.to_list pieces |> String.concat "" in
          if Word160.equal (Word160.digest_of_string all) info_hash then begin
            (* Trace.infof "%s: info data validated: %S" (to_string self) s; *)
            Get.run Bcode.bdecode all |> Info.create |> on_completion;
            self#stop
          end else begin
            Trace.infof "%t: info data invalid, stopping" self#sprint;
            self#stop
          end
        end else
          self#request_info_piece (piece+1)
      | 2, _ -> (* reject *)
        Trace.infof "%t: rejected request for info piece %d, stopping"
          self#sprint piece;
        self#stop
      | _ ->
        (* An unrecognised msg_type must be ignored, see
           http://www.bittorrent.org/beps/bep_0009.html, 'extension message' *)
        Trace.infof "%t: unknown ut_metadata msg_type %d, ignoring"
          self#sprint msg_type

    method got_extended_handshake bc =
      super#got_extended_handshake bc;
      if Hashtbl.mem extensions "ut_metadata" then begin
        let l = Bcode.find "metadata_size" bc |> Bcode.to_int in
        let numpieces = roundup l info_piece_size / info_piece_size in
        let last_piece_size = l mod info_piece_size in
        Trace.infof "%t: got metadata size: %d, %d pieces, last piece: %d"
          self#sprint l numpieces last_piece_size;
        let pieces = Array.init numpieces (fun i ->
            if i < numpieces - 1 then String.make info_piece_size '\000'
            else String.make last_piece_size '\000') in
        length <- Some pieces;
        self#request_info_piece 0
      end else begin
        Trace.infof "%t does not support ut_metadata, stopping" self#sprint;
        self#stop
      end

    method get_block _ _ _ =
      assert false
        
    method got_have _ = ()
    method got_have_bitfield _ = ()
    method got_have_all = ()
    method got_request _ _ _ = ()
    method got_piece _ _ _ = ()
  end

module S3 = Set.Make (struct type t = (int * int * int) let compare = compare end)
    
class sharing fd info_hash info picker torrent (id, exts) =
  object (self)
    val have = Bits.create (Array.length info.Info.pieces)
    val mutable active_requests : S3.t = S3.empty

    inherit peer fd (id, exts) as super

    method get_block i o l =
      let (>|=) = Lwt.(>|=) in
      Torrent.get_block torrent i o l >|= function
      | None -> failwith "get_block: invalid block: i=%d o=%d l=%d" i o l
      | Some s -> s

    method sprint () =
      Printf.sprintf "%t [%d/%d]" super#sprint (Bits.count have) (Bits.length have)

    method got_ut_metadata msg_type piece s =
      let id = Hashtbl.find extensions "ut_metadata" in
      match msg_type with
      | 0 -> (* request *)
        let d =
          [ "msg_type", Bcode.BInt 2L;
            "piece", Bcode.BInt (Int64.of_int piece);
            "total_size", Bcode.BInt (Int64.of_int (Info.length info)) ]
        in
        let s =
          Info.get_piece info piece |> Put.string |>
          Put.bind (Bcode.bencode (Bcode.BDict d)) |> Put.run
        in
        Wire.EXTENDED (id, s) |> self#send_message
      | 1
      | 2 ->
        failwith "%t: got_ut_metadata: bad msg_type: %d" self#sprint msg_type
      | _ ->
        ()

    method got_have_all =
      for i = 0 to Bits.length have do
        Bits.set have i;
        Picker.got_have picker i
      done

    method got_have i =
      if not (Bits.is_set have i) then begin
        Bits.set have i;
        Picker.got_have picker i
      end

    method got_choke =
      super#got_choke;
      S3.iter (fun (i, o, l) -> Torrent.request_lost torrent i o l) active_requests

    method want i =
      Bits.is_set have i && Torrent.available_requests torrent i

    method private request_more =
      let rec loop () =
        if S3.cardinal active_requests >= request_backlog then ()
        else
          match Picker.next picker self#want with
          | Some i ->
            begin match Torrent.new_request torrent i with
              | Some (o, l) ->
                active_requests <- S3.add (i, o, l) active_requests;
                self#send_message (Wire.REQUEST (i, o, l))
              | None ->
                ()
            end
          | None ->
            ()
      in
      loop ()

    method got_unchoke =
      super#got_unchoke;
      if am_interested then self#request_more

    method got_have_bitfield b =
      Bits.blit b 0 have 0 (Bits.length have);
      for i = 0 to Bits.length have do
        if Bits.is_set have i then Picker.got_have picker i
      done

    method got_piece i o s =
      let l = String.length s in
      if S3.mem (i, o, l) active_requests then begin
        active_requests <- S3.remove (i, o, l) active_requests;
        Torrent.got_block torrent i o s |> ignore;
        self#request_more
      end
      else
        failwith "%t sent unrequested block, terminating" self#sprint

    method got_request i o l =
      if not am_choking || peer_interested then
        self#send_block i o l
      else if fastext then
        self#send_message (Wire.REJECT (i, o, l))
  end
  
(* type t = { *)
(*   sa : Lwt_unix.sockaddr; *)
(*   id : Word160.t; *)
(*   status : peer_status; *)
(*   mutable am_interested : bool; *)
(*   mutable peer_interested : bool; *)
(*   mutable am_choking : bool; *)
(*   mutable peer_choking : bool; *)
(*   stop : unit -> unit; *)
(*   (\* write : [ `Literal of string | `Block of int * int * int ] -> unit; *\) *)
(*   ul : float Lwt_react.S.t; *)
(*   dl : float Lwt_react.S.t; *)
(*   update_dl : int -> unit; *)
(*   update_ul : int -> unit; *)
(*   fastext : bool; *)
(*   ltext : bool; *)
(*   extensions : (string, int) Hashtbl.t *)
(* } *)

(* let string_of_sockaddr = function *)
(*   | Lwt_unix.ADDR_UNIX s -> *)
(*     s *)
(*   | Lwt_unix.ADDR_INET (addr, port) -> *)
(*     Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port *)

(* let stop self = *)
(*   Trace.infof "STOPPING %s" (to_string self); *)
(*   self.stop () *)

(* let send_message self message = *)
(*   let s = Wire.put message |> Put.run in *)
(*   Trace.sent (to_string self) (Wire.string_of_message message); *)
(*   match self.status with *)
(*   | INFO info -> *)
(*     s |> info.info_write *)
(*   | DOWNLOAD dl -> *)
(*     `LIT s |> dl.dl_write *)

(* let send_block self i off len = *)
(*   match self.status with *)
(*   | INFO _ -> *)
(*     assert false *)
(*   | DOWNLOAD dl -> *)
(*     `BLOCK (i, off, len) |> dl.dl_write *)
      
(* let got_have self i = *)
(*   match self.status with *)
(*   | DOWNLOAD dl -> *)
(*     if i < 0 || i >= Bits.length dl.have then *)
(*       failwith "%s has invalid #%d" (to_string self) i; *)
(*     Trace.infof "%s has obtained #%d and now has %d/%d pieces" *)
(*       (to_string self) i (Bits.count dl.have) (Bits.length dl.have); *)
(*     Bits.set dl.have i; *)
(*     Picker.got_have dl.picker i *)
(*   | INFO _ -> *)
(*     () *)

(* let got_request self i off len = *)
(*   if self.am_choking then *)
(*     failwith "%s violating protocol, terminating exchange" (to_string self); *)
(*   (\* FIXME *\) *)
(*   match self.status with *)
(*   | DOWNLOAD _ -> *)
(*     send_block self i off len *)
(*   | INFO _ -> *)
(*     () *)

(* let got_piece self i off s = *)
(*   match self.status with *)
(*   | INFO _ -> *)
(*     (\* PROTOOCL VIOLATION *\) *)
(*     stop self *)
(*   | DOWNLOAD dl -> *)
(*     let len = String.length s in *)
(*     if Hashset.mem dl.active_requests (i, off, len) then begin *)
(*       Hashset.remove dl.active_requests (i, off, len); *)
(*       Torrent.got_block dl.torrent i off s; *)
(*       request_more self *)
(*     end else *)
(*       failwith "%s sent unrequested block, terminating" (to_string self) *)

(* let roundup n r = *)
(*   (n + r - 1) / r * r *)

(* let got_extended self id m = *)
(*   if List.mem_assoc id supported_extensions then *)
(*     let _, f = List.assoc id supported_extensions in *)
(*     f self m *)
(*   else       *)
(*     Trace.infof "%t: unsupported EXTENDED message %d" (sprint self) id *)

(* let got_message self message = *)
(*   Trace.recv (to_string self) (Wire.string_of_message message); *)
(*   match message with *)
(*   | Wire.KEEP_ALIVE -> () *)
(*   | Wire.CHOKE -> got_choke self *)
(*   | Wire.UNCHOKE -> got_unchoke self *)
(*   | Wire.INTERESTED -> got_interested self *)
(*   | Wire.NOT_INTERESTED -> got_not_interested self *)
(*   | Wire.HAVE i -> got_have self i *)
(*   | Wire.BITFIELD bits -> *)
(*     if self.ltext then got_have_bitfield self bits else stop self *)
(*   | Wire.REQUEST (i, off, len) -> got_request self i off len *)
(*   | Wire.PIECE (i, off, s) -> got_piece self i off s *)
(*   | Wire.CANCEL (i, off, len) -> () *)
(*   | Wire.HAVE_ALL *)
(*   | Wire.HAVE_NONE -> stop self *)
(*   | Wire.EXTENDED (0, m) -> got_extended_handshake self m *)
(*   | Wire.EXTENDED (id, m) -> got_extended self id m *)
(*   | _ -> *)
(*     Trace.infof "Unhandled message from %t: %t" (sprint self) (Wire.sprint message); *)
(*     stop self *)

(* let got_first_message self message = *)
(*   Trace.recv (to_string self) (Wire.string_of_message message); *)
(*   match message with *)
(*   | Wire.BITFIELD bits -> got_have_bitfield self bits *)
(*   | Wire.HAVE_ALL -> if self.fastext then got_have_all self else stop self *)
(*   | Wire.HAVE_NONE -> if self.fastext then () else stop self *)
(*   | _ -> if self.fastext then stop self else got_message self message *)

(* let piece_completed self i = *)
(*     send_message self (Wire.HAVE i) *)

(* let is_interested self = *)
(*   self.peer_interested *)

(* let is_choked self = *)
(*   self.am_choking *)

(* (\* let interesting self = *\) *)
(* (\*   if not self.interesting then begin *\) *)
(* (\*     self.we_interested <- true; *\) *)
(* (\*     send_message self Wire.INTERESTED *\) *)
(* (\*   end *\) *)

(* (\* let not_interesting self = *\) *)
(* (\*   if self.we_interested then begin *\) *)
(* (\*     self.we_interested <- false; *\) *)
(* (\*     send_message self Wire.NOT_INTERESTED *\) *)
(* (\*   end *\) *)

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

(* let (>>=) = Lwt.(>>=) *)
(* let (>|=) = Lwt.(>|=) *)

(* let writer_loop_partial oc write_queue stop_thread stop = *)
(*   let keepalive_message = Wire.put Wire.KEEP_ALIVE |> Put.run in *)
(*   let rec loop () = *)
(*     Lwt.pick *)
(*       [ Lwt_stream.next write_queue >|= (fun x -> `Write x); *)
(*         stop_thread >|= (fun () -> `Stop); *)
(*         Lwt_unix.sleep (float keepalive_delay) >|= (fun () -> `Timeout) ] >>= function *)
(*     | `Write s -> *)
(*       Lwt_io.write oc s >>= loop *)
(*     | `Stop -> *)
(*       Lwt.return_unit *)
(*     | `Timeout -> *)
(*       Lwt_io.write oc keepalive_message >>= loop *)
(*   in *)
(*   Lwt.catch loop *)
(*     (fun exn -> *)
(*        Trace.infof ~exn "Error in writer loop"; *)
(*        Lwt.wrap stop) >>= fun () -> Lwt_io.close oc *)
    
(* (\* let writer_loop oc get_block write_queue stop_thread stop = *\) *)
(* (\*   let keepalive_message = Wire.put Wire.KEEP_ALIVE |> Put.run in *\) *)
(* (\*   let rec loop () = *\) *)
(* (\*     Lwt.pick *\) *)
(* (\*       [ Lwt_stream.next write_queue >|= (fun x -> `Write x); *\) *)
(* (\*         stop_thread >|= (fun () -> `Stop); *\) *)
(* (\*         Lwt_unix.sleep (float keepalive_delay) >|= (fun () -> `Timeout) ] >>= function *\) *)
(* (\*     | `Write (`Literal s) -> *\) *)
(* (\*       Lwt_io.write oc s >>= loop *\) *)
(* (\*     | `Write (`Block (i, off, len)) -> *\) *)
(* (\*       get_block i off len >>= begin function *\) *)
(* (\*         | None -> *\) *)
(* (\*           Lwt.wrap stop *\) *)
(* (\*         | Some s -> *\) *)
(* (\*           Wire.PIECE (i, off, s) |> Wire.put |> Put.run |> Lwt_io.write oc >>= loop *\) *)
(* (\*       end *\) *)
(* (\*     | `Stop -> *\) *)
(* (\*       Lwt.return_unit *\) *)
(* (\*     | `Timeout -> *\) *)
(* (\*       Lwt_io.write oc keepalive_message >>= loop *\) *)
(* (\*   in *\) *)
(* (\*   Lwt.catch loop *\) *)
(* (\*     (fun exn -> *\) *)
(* (\*        Trace.infof ~exn "Error in writer loop"; *\) *)
(* (\*        Lwt.wrap stop) >>= fun () -> Lwt_io.close oc *\) *)

(* let reader_loop ic got_first_message got_message stop_thread stop = *)
(*   let input = Lwt_stream.from (fun () -> Wire.read ic >|= fun msg -> Some msg) in *)
(*   let rec loop gotmsg = *)
(*     Lwt.pick *)
(*       [ Lwt_stream.next input >|= (fun x -> `Ok x); *)
(*         stop_thread >|= (fun () -> `Stop); *)
(*         Lwt_unix.sleep (float keepalive_delay) >|= (fun () -> `Timeout) ] >>= function *)
(*     | `Ok x -> *)
(*       gotmsg x; *)
(*       loop got_message *)
(*     | `Stop -> *)
(*       Lwt.return_unit *)
(*     | `Timeout -> *)
(*       Lwt.wrap stop *)
(*   in *)
(*   Lwt.catch *)
(*     (fun () -> loop got_first_message) *)
(*     (fun exn -> *)
(*        Trace.infof ~exn "Error in reader loop"; *)
(*        Lwt.wrap stop) >>= fun () -> Lwt_io.close ic *)

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

(* let partial_exts = *)
(*   let b = Bits.create (8 * 8) in *)
(*   Bits.set b ltextbit; *)
(*   b *)

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
