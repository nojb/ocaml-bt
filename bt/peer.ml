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
  | Choked
  | Have of int
  | HaveBitfield of Bits.t
  | BlockRequested of int * int * int
  | BlockReceived of int * int * string
  | Port of int
  | Finished
  | AvailableMetadata of int
  | MetaRequested of int
  | GotMetaPiece of int * string
  | RejectMetaPiece of int

type event_callback = event -> unit
type get_metadata_func = unit -> int option
type get_block_func = int -> (int * int) list

let kilobyte n = n * 1024
let keepalive_delay = 20 (* FIXME *)
let request_pipeline_max = 5
let info_piece_size = kilobyte 16
let default_block_size = kilobyte 16

exception Timeout

type has_meta_info = {
  have : Bits.t;
  blame : Bits.t;
  request : get_block_func;
  meta : Metadata.t
}

type no_meta_info = {
  mutable have : int list;
  mutable has_all : bool;
  request : get_metadata_func
}

type meta_info =
  | HasMeta of has_meta_info
  | NoMeta of no_meta_info

type t = {
  addr : Addr.t;
  sock : IO.socket;
  output : Lwt_io.output_channel;
  input : Lwt_io.input_channel;
  mutable id : SHA1.t;
  mutable am_choking : bool;
  mutable am_interested : bool;
  mutable peer_choking : bool;
  mutable peer_interested : bool;
  should_stop : unit Lwt.t;
  extbits : Bits.t;
  extensions : (string, int) Hashtbl.t;
  
  mutable act_reqs : int;
  
  send_queue : Wire.message Lwt_sequence.t;
  send_waiters : Wire.message Lwt.u Lwt_sequence.t;
  
  handle : event -> unit;

  mutable info : meta_info;
  (* have : bits; *)
  (* blame : Bits.t; *)

  mutable upload : Rate.t;
  mutable download : Rate.t;

  (* request : request_func *)

  (* mutable get_next_requests : (int -> (int * int * int) list) option; *)
  (* mutable get_next_metadata_request : (unit -> int option) option *)
}

let next_to_send p =
  if Lwt_sequence.length p.send_queue > 0 then
    Lwt.return (Lwt_sequence.take_l p.send_queue)
  else
    Lwt.add_task_r p.send_waiters

let send_message p m =
  if Lwt_sequence.length p.send_waiters > 0 then begin
    assert (Lwt_sequence.is_empty p.send_queue);
    Lwt.wakeup (Lwt_sequence.take_l p.send_waiters) m
  end
  else
    ignore (Lwt_sequence.add_r m p.send_queue)

let signal p e =
  p.handle e
  (* match p.handle with *)
  (* | None -> () *)
  (* | Some h -> h e *)

let send_extended p id s =
  send_message p (Wire.EXTENDED (id, s))

let send_block p i o s =
  send_message p (Wire.PIECE (i, o, s))

let got_ut_metadata p data =
  let m, data_start = Bcode.decode_partial data in
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
    let d = [ "msg_type", Bcode.Int 2L; "piece", Bcode.Int (Int64.of_int piece) ] in
    Bcode.encode (Bcode.Dict d)
  in
  send_extended p id m

let send_meta_piece p piece (len, s) =
  let id = Hashtbl.find p.extensions "ut_metadata" in
  let m =
    let d =
      [ "msg_type", Bcode.Int 1L;
        "piece", Bcode.Int (Int64.of_int piece);
        "total_size", Bcode.Int (Int64.of_int len) ] in
    Bcode.encode (Bcode.Dict d) ^ s
  in
  send_extended p id m
  
let supported_extensions =
  [ 1, ("ut_metadata", got_ut_metadata) ]

let got_choke p =
  if not p.peer_choking then begin
    p.peer_choking <- true;
    p.act_reqs <- 0;
    signal p Choked
  end

let got_unchoke p =
  if p.peer_choking then begin
    p.peer_choking <- false
    (* signal p Unchoked *)
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
  | HasMeta n ->
    Bits.blit b 0 n.have 0 (Bits.length n.have);
    signal p (HaveBitfield n.have)
  | NoMeta n ->
    let rec loop acc i =
      if i >= Bits.length b then List.rev acc else
      if Bits.is_set b i then loop (i :: acc) (i+1)
      else loop acc (i+1)
    in
    n.have <- loop [] 0;
    signal p (HaveBitfield b)
  end

let got_have p idx =
  match p.info with
  | HasMeta nfo ->
    if not (Bits.is_set nfo.have idx) then begin
      Bits.set nfo.have idx;
      signal p (Have idx)
    end
  | NoMeta n ->
    if not (List.mem idx n.have) && not n.has_all then begin
      n.have <- idx :: n.have;
      signal p (Have idx)
    end
  
let got_cancel p i ofs len =
  Lwt_sequence.iter_node_l (fun n ->
      match Lwt_sequence.get n with
      | Wire.PIECE (i1, ofs1, s) when i = i1 && ofs = ofs1 && String.length s = len ->
        Lwt_sequence.remove n
      | _ ->
        ()) p.send_queue

let got_piece p idx off s =
  p.act_reqs <- p.act_reqs - 1;
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
  | Wire.EXTENDED (0, s) -> got_extended_handshake p (Bcode.decode s)
  | Wire.EXTENDED (id, s) -> got_extended p id s
  | _ -> assert false

let reader_loop p =
  let input = Lwt_stream.from
      (fun () -> Wire.read p.input >>= fun msg ->
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
  (* p.act_reqs <- (i, ofs, len) :: p.act_reqs; *)
  p.act_reqs <- p.act_reqs + 1;
  send_message p (Wire.REQUEST (i, ofs, len))

let writer_loop p =
  let rec loop () =
    Lwt.pick [
      (Lwt_unix.sleep (float keepalive_delay) >>= fun () -> Lwt.return `Timeout);
      (p.should_stop >>= fun () -> Lwt.return `Stop);
      (next_to_send p >|= fun msg -> `Ready msg)
    ]
    >>= function
    | `Timeout ->
      Wire.write p.output Wire.KEEP_ALIVE >>= fun () ->
      Lwt_io.flush p.output >>= loop
    | `Stop ->
      Lwt.return_unit
    | `Ready m ->
      Wire.write p.output m >>= fun () ->
      Lwt_io.flush p.output >>= fun () ->
      Log.debug "%s <<< %s" (Addr.to_string p.addr) (Wire.string_of_message m);
      (match m with Wire.PIECE (_, _, s) -> Rate.add p.upload (String.length s) | _ -> ());
      loop ()
  in
  loop ()

let supports_ut_metadata p =
  Hashtbl.mem p.extensions "ut_metadata"

let id p =
  p.id

let addr p =
  p.addr

let send_extended_handshake p =
  let m =
    List.map (fun (id, (name, _)) ->
        name, Bcode.Int (Int64.of_int id)) supported_extensions
  in
  let m = Bcode.Dict ["m", Bcode.Dict m] in
  send_extended p 0 (Bcode.encode m)

let peer_choking p =
  p.peer_choking

let peer_interested p =
  p.peer_interested

let has_piece p idx =
  assert (0 <= idx);
  match p.info with
  | HasMeta nfo -> Bits.is_set nfo.have idx
  | NoMeta nfo -> List.mem idx nfo.have
  (* if idx >= Bits.length p.have then false *)
  (* else Bits.is_set p.have idx *)

let have p =
  match p.info with
  | HasMeta nfo -> Bits.copy nfo.have
  | NoMeta _ -> failwith "Peer.have: no meta info"

let am_choking p =
  p.am_choking

let client_interested p =
  p.am_interested

let send_choke p =
  if not p.am_choking then begin
    p.am_choking <- true;
    Log.info "choking peer (addr=%s)" (Addr.to_string p.addr);
    send_message p Wire.CHOKE
  end

let send_unchoke p =
  if p.am_choking then begin
    p.am_choking <- false;
    Log.info "unchoking peer (addr=%s)" (Addr.to_string p.addr);
    send_message p Wire.UNCHOKE
  end

let send_interested p =
  if not p.am_interested then begin
    p.am_interested <- true;
    Log.info "interested in peer (addr=%s)" (Addr.to_string p.addr);
    send_message p Wire.INTERESTED
  end

let send_not_interested p =
  if p.am_interested then begin
    p.am_interested <- false;
    Log.info "not interested in peer (addr=%s)" (Addr.to_string p.addr);
    send_message p Wire.NOT_INTERESTED
  end

let send_have p idx =
  if not (has_piece p idx) then (* Bits.is_set p.have idx) then*)
    send_message p (Wire.HAVE idx)

let send_have_bitfield p bits =
  for i = 0 to Bits.length bits - 1 do
    if Bits.is_set bits i then send_have p i
  done

let send_cancel p (i, j) =
  match p.info with
  | HasMeta n ->
    let i, ofs, len = Metadata.block n.meta i j in
    send_message p (Wire.CANCEL (i, ofs, len))
  | NoMeta _ ->
    failwith "send_cancel: no meta info"

let request_meta_piece p idx =
  assert (idx >= 0);
  assert (Hashtbl.mem p.extensions "ut_metadata");
  let id = Hashtbl.find p.extensions "ut_metadata" in
  let d =
    [ "msg_type", Bcode.Int 0L;
      "piece", Bcode.Int (Int64.of_int idx) ]
  in
  Bcode.encode (Bcode.Dict d) |> send_extended p id
    
let upload_rate p = Rate.get p.upload
let download_rate p = Rate.get p.download

let reset_rates p = Rate.reset p.upload; Rate.reset p.download

let request_metadata_loop p =
  let rec loop () =
    match p.info with
    | HasMeta _ ->
      Lwt.return ()
    | NoMeta nfo ->
      if supports_ut_metadata p then begin
        begin match nfo.request () with
        | Some i -> request_meta_piece p i
        | None -> ()
        end;
        (* p.sock#on_write >>= loop *)
        Lwt_unix.sleep 1.0 >>= loop
      end
      else
        Lwt_unix.sleep 5.0 >>= loop
  in
  loop ()

let request_loop p =
  let rec loop () =
    match p.info with
    | HasMeta nfo ->
      let ps = nfo.request (request_pipeline_max - p.act_reqs) in
      List.iter (fun (i, j) -> send_request p (Metadata.block nfo.meta i j)) ps;
      (* p.sock#on_write >>= loop *)
      Lwt_unix.sleep 1.0 >>= loop
    | NoMeta _ ->
      failwith "Peer.request_loop: no meta info"
  in
  loop ()

let start p =
  let run_loop () =
    Lwt.catch
      (fun () -> Lwt.join [reader_loop p; writer_loop p])
      (fun e ->
         Log.error ~exn:e "peer input/output error (addr=%s,id=%s)"
           (Addr.to_string p.addr) (SHA1.to_hex_short p.id);
         Lwt.return ())
    >>= fun () ->
    p.act_reqs <- 0;
    signal p Finished;
    Lwt.return ()
  in
  Lwt.async run_loop

let got_metadata p m get_next_requests =
  match p.info with
  | NoMeta nfo ->
    let n = Metadata.piece_count m in
    let have = Bits.create n in
    if nfo.has_all then Bits.set_all have else List.iter (Bits.set have) nfo.have;
    let info =
      { have;
        blame = Bits.create n;
        request = get_next_requests;
        meta = m }
    in
    p.info <- HasMeta info;
    Lwt.async (fun () -> request_loop p)
  | HasMeta _ ->
    failwith "Peer.got_metadata: already has meta"

let create_no_meta sock addr id handle_event get_next_metadata_request =
  let w, wake = Lwt.wait () in
  let input, output = IO.in_channel sock, IO.out_channel sock in
  let extensions = Hashtbl.create 17 in
  let p =
    { sock; addr; input; output; id;
      am_choking = true; am_interested = false;
      peer_choking = true; peer_interested = false;
      should_stop = w; extbits = Bits.create (8 * 8);
      extensions;
      act_reqs = 0; 
      send_queue = Lwt_sequence.create ();
      send_waiters = Lwt_sequence.create ();
      handle = handle_event;
      info = NoMeta { have = []; has_all = false; request = get_next_metadata_request };
      download = Rate.create ();
      upload = Rate.create () }
  in
  Lwt.async (fun () -> request_metadata_loop p);
  p
  
let create_has_meta sock addr id handle_event m get_next_requests =
  let w, wake = Lwt.wait () in
  let input, output = IO.in_channel sock, IO.out_channel sock in
  let extensions = Hashtbl.create 17 in
  let npieces = Metadata.piece_count m in
  let p =
    { sock; addr; input; output; id;
      am_choking = true; am_interested = false;
      peer_choking = true; peer_interested = false;
      should_stop = w; extbits = Bits.create (8 * 8);
      extensions;
      act_reqs = 0; 
      send_queue = Lwt_sequence.create ();
      send_waiters = Lwt_sequence.create ();
      handle = handle_event;
      info = HasMeta
          { have = Bits.create npieces;
            blame = Bits.create npieces; request = get_next_requests; meta = m };
      download = Rate.create ();
      upload = Rate.create () }
  in
  Lwt.async (fun () -> request_loop p);
  p
