let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let failwith fmt = Printf.ksprintf failwith fmt
let failwith_lwt fmt = Printf.ksprintf (fun msg -> Lwt.fail (Failure msg)) fmt
let invalid_arg_lwt s = Lwt.fail (Invalid_argument s)
    
type store_state =
  | INITIALIZING of (unit -> unit)
  | READY of Lwt_mutex.t * (Lwt_unix.file_descr * int64) list
  | CLOSED
  | ERROR of exn

type t = {
  files : Info.file_info list;
  mutable state : store_state
}

let safe_to_int n =
  let n' = Int64.to_int n in
  assert (Int64.(compare (of_int n') n) = 0);
  n'

let rec get_chunks handles offset size =
  if size < 0 || Int64.compare offset 0L < 0 then
    invalid_arg "Store.get_chunks"
  else
    match handles with
    | [] ->
      invalid_arg "Store.get_chunks"
    | (fd, size1) :: handles ->
      let open Int64 in
      if compare offset size1 >= 0 then
        get_chunks handles (sub offset size1) size
      else begin
        let size1 = sub size1 offset in
        if compare size1 (of_int size) >= 0 then
          [fd, offset, size]
        else
          let size1 = safe_to_int size1 in
          (fd, offset, size1) :: get_chunks handles 0L (size-size1)
      end

let read_exactly fd buf off len =
  Lwt_unix.read fd buf off len >>= fun len' ->
  if len <> len' then failwith_lwt "Store.read_exactly %d bytes when expecting %d bytes" len' len
  else Lwt.return_unit

let _read lock handles off len =
  let buf = String.create len in
  let read_chunk doff (fd, off, len) =
    Lwt_unix.LargeFile.lseek fd off Unix.SEEK_SET >>= fun _ ->
    read_exactly fd buf doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  Lwt_mutex.with_lock lock (fun () ->
      Lwt_list.fold_left_s read_chunk 0 (get_chunks handles off len) >>= fun n ->
      if n <> len then
        failwith_lwt "Store._read: incomplete read (%d instead of %d)" n len
      else
        Lwt.return buf)

let rec write_exactly fd buf off len =
  Lwt_unix.write fd buf off len >>= fun len' ->
  if len' < len then begin
    Trace.infof "Store.write_exactly: incomplete buffer write; iterating...";
    write_exactly fd buf (off+len') (len-len')
  end else
    Lwt.return_unit

let _write lock handles off buf =
  let write_chunk doff (fd, off, len) =
    Lwt_unix.LargeFile.lseek fd off Unix.SEEK_SET >>= fun _ ->
    write_exactly fd buf doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  Lwt_mutex.with_lock lock (fun () ->
      Lwt_list.fold_left_s write_chunk 0 (get_chunks handles off (String.length buf)) >>= fun n ->
      if n <> (String.length buf) then
        failwith_lwt "Store._write: incomplete write (%d instead of %d)" n (String.length buf)
      else
        Lwt.return_unit)

let write store off buf =
  match store.state with
  | READY (lock, handles) ->
    _write lock handles off buf
  | _ ->
    failwith_lwt "Store.write: store not ready"

let read store off len =
  match store.state with
  | READY (lock, handles) ->
    _read lock handles off len
  | _ ->
    failwith_lwt "Store.read: store not ready"
      
let cwd path f =
  let old_cwd = Sys.getcwd () in
  Lwt_unix.chdir path >>= fun () ->
  Lwt.finalize f (fun () -> Lwt_unix.chdir old_cwd)

let file_default_perm = 0o644
let directory_default_perm = 0o755
  
let open_file path size =
  let rec loop path =
    match path with
    | [] ->
      invalid_arg_lwt "Store.open_file"
    | [path] ->
      Lwt_unix.openfile path [Unix.O_CREAT; Unix.O_RDWR] file_default_perm >>= fun fd ->
      Lwt_unix.LargeFile.ftruncate fd size >>= fun () ->
      Lwt.return fd
    | dir :: path ->
      Lwt.try_bind
        (fun () -> Lwt_unix.LargeFile.stat dir)
        (fun st ->
           match st.Unix.LargeFile.st_kind with
           | Unix.S_DIR ->
             cwd dir (fun () -> loop path)
           | _ ->
             failwith_lwt "Store.open_file: %S exists and is not a directory"
               (Filename.concat (Sys.getcwd ()) dir))
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) ->
            Trace.infof "Directory %S does not exist; creating..." dir;
            Lwt_unix.mkdir dir directory_default_perm >>= fun () ->
            cwd dir (fun () -> loop path)
          | exn ->
            Lwt.fail exn)
  in
  loop path

let create files =
  let self = { files; state = INITIALIZING (fun () -> ()) } in
  Trace.infof "Initializing Store...";
  Lwt.try_bind
    (fun () ->
       Lwt_list.map_s (fun fi ->
           open_file fi.Info.file_path fi.Info.file_size >>= fun fd ->
           Lwt.return (fd, fi.Info.file_size)) self.files)
    (fun handles ->
       self.state <- READY (Lwt_mutex.create (), handles); Lwt.return self)
    (fun exn ->
       self.state <- ERROR exn; Lwt.return self)

let _close handles =
  Lwt_list.iter_p (fun (fd, _) -> Lwt_unix.close fd) handles

let close store =
  match store.state with
  | INITIALIZING abort ->
    abort ();
    store.state <- CLOSED
  | READY (lock, handles) ->
    Lwt.catch
      (fun () ->
         Lwt_mutex.with_lock lock (fun () -> _close handles) >|= fun () ->
         store.state <- CLOSED)
      (fun exn ->
         store.state <- ERROR exn; Lwt.return_unit) |> ignore
  | CLOSED
  | ERROR _ ->
    ()
