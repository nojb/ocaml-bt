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

let failwith fmt = Printf.ksprintf failwith fmt
let failwith_lwt fmt = Printf.ksprintf (fun msg -> Lwt.fail (Failure msg)) fmt
let invalid_arg_lwt s = Lwt.fail (Invalid_argument s)

type t =
  { descrs : (Lwt_unix.file_descr * int64) list;
    lock : Lwt_mutex.t }

let rec get_chunks descrs offset size =
  let open Int64 in
  assert (size >= 0 && compare offset 0L >= 0);
  match descrs with
  | [] -> []
  | (fd, size1) :: descrs ->
    if compare offset size1 >= 0 then
      get_chunks descrs (sub offset size1) size
    else begin
      let size1 = sub size1 offset in
      if compare size1 (of_int size) >= 0 then
        [fd, offset, size]
      else
        let size1 = Int64.to_int size1 in
        (fd, offset, size1) :: get_chunks descrs 0L (size-size1)
    end

let read self off len =
  let read_chunk buf (fd, off, len) =
    Lwt_unix.LargeFile.lseek fd off Unix.SEEK_SET >>= fun _ ->
    Lwt_cstruct.complete (Lwt_cstruct.read fd) (Cstruct.sub buf 0 len) >>= fun () ->
    Lwt.return (Cstruct.shift buf len)
  in
  let buf = Cstruct.create len in
  Lwt_mutex.with_lock self.lock
    (fun () ->
       Lwt_list.fold_left_s read_chunk buf (get_chunks self.descrs off len) >>= fun _ ->
       Lwt.return buf)

let write self off buf =
  let write_chunk buf (fd, off, len) =
    Lwt_unix.LargeFile.lseek fd off Unix.SEEK_SET >>= fun _ ->
    Lwt_cstruct.complete (Lwt_cstruct.write fd) (Cstruct.sub buf 0 len) >>= fun () ->
    Lwt.return (Cstruct.shift buf len)
  in
  Lwt_mutex.with_lock self.lock
    (fun () ->
       Lwt_list.fold_left_s write_chunk buf (get_chunks self.descrs off (Cstruct.len buf)) >>= fun _ ->
       Lwt.return_unit)

let cwd path f =
  let old_cwd = Sys.getcwd () in
  Lwt_unix.chdir path >>= fun () ->
  Lwt.finalize f (fun () -> Lwt_unix.chdir old_cwd)

let file_default_perm = 0o600
let directory_default_perm = 0o700

let open_file path size =
  let rec loop path =
    match path with
    | [] ->
      assert false
    | [path] ->
      Lwt_unix.openfile path [Unix.O_CREAT; Unix.O_RDWR] file_default_perm >>= fun fd ->
      Lwt_unix.LargeFile.ftruncate fd size >>= fun () ->
      Lwt.return fd
    | dir :: path ->
      if Sys.file_exists dir then
        cwd dir (fun () -> loop path)
      else begin
        (* Trace.infof "Directory %S does not exist; creating..." dir; *)
        Lwt_unix.mkdir dir directory_default_perm >>= fun () ->
        cwd dir (fun () -> loop path)
      end
  in
  loop path

let close self =
  Lwt_mutex.with_lock self.lock
    (fun () -> Lwt_list.iter_p (fun (fd, _) -> Lwt_unix.close fd) self.descrs)

let create files =
  Lwt_list.map_s
    (fun (path, size) -> open_file path size >>= fun fd -> Lwt.return (fd, size)) files >>= fun descrs ->
  Lwt.return { descrs; lock = Lwt_mutex.create () }

(* let add_file self path size = *)
(*   open_file path size >>= fun fd -> *)
(*   self.descrs <- self.descrs @ [fd, size]; *)
(*   Lwt.return () *)

let update self =
  assert false
