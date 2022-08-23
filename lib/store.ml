(* The MIT License (MIT)

   Copyright (c) 2013-2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

open Lwt.Infix

type t = (Lwt_unix.file_descr * int64 * Lwt_mutex.t) list

let get_chunks files offset size =
  if size < 0 || offset < 0L then invalid_arg "Store.get_chunks";
  let open Int64 in
  let rec loop offset size = function
    | [] -> []
    | (fd, size1, m) :: files ->
        if offset >= size1 then loop (sub offset size1) size files
        else
          let size1 = sub size1 offset in
          if size1 >= of_int size then [ (fd, offset, size, m) ]
          else
            let size1 = Int64.to_int size1 in
            (fd, offset, size1, m) :: loop 0L (size - size1) files
  in
  loop offset size files

let buf = Cstruct.create 1024

let digest' fd len sha =
  if len < 0 then invalid_arg "Store.digest'";
  let buf_len = Cstruct.length buf in
  let rec loop len =
    if len > 0 then
      let buf = Cstruct.sub buf 0 (min len buf_len) in
      Lwt_cstruct.complete (Lwt_cstruct.read fd) buf >>= fun () ->
      Lwt.wrap2 Nocrypto.Hash.SHA1.feed sha buf >>= fun () ->
      loop (len - Cstruct.length buf)
    else Lwt.return_unit
  in
  loop len

let digest files off len =
  if len < 0 then invalid_arg "Store.digest";
  let chunks = get_chunks files off len in
  let sha = Nocrypto.Hash.SHA1.init () in
  Lwt_list.iter_s
    (fun (fd, off, len, m) ->
      Lwt_mutex.with_lock m (fun () ->
          Lwt_unix.LargeFile.lseek fd off Unix.SEEK_SET >>= fun _ ->
          digest' fd len sha))
    chunks
  >>= fun () -> Lwt.wrap1 SHA1.of_raw (Nocrypto.Hash.SHA1.get sha)

let read files off len =
  let read_chunk buf (fd, off, len, m) =
    Lwt_mutex.with_lock m (fun () ->
        Lwt_unix.LargeFile.lseek fd off Unix.SEEK_SET >>= fun _ ->
        Lwt_cstruct.complete (Lwt_cstruct.read fd) (Cstruct.sub buf 0 len)
        >>= fun () -> Lwt.return (Cstruct.shift buf len))
  in
  let buf = Cstruct.create len in
  Lwt_list.fold_left_s read_chunk buf (get_chunks files off len) >>= fun _ ->
  Lwt.return buf

let write files off buf =
  let write_chunk buf (fd, off, len, m) =
    Lwt_mutex.with_lock m (fun () ->
        Lwt_unix.LargeFile.lseek fd off Unix.SEEK_SET >>= fun _ ->
        Lwt_cstruct.complete (Lwt_cstruct.write fd) (Cstruct.sub buf 0 len)
        >>= fun () -> Lwt.return (Cstruct.shift buf len))
  in
  Lwt_list.fold_left_s write_chunk buf
    (get_chunks files off (Cstruct.length buf))
  >>= fun _ -> Lwt.return_unit

let cd path f =
  let old_cwd = Sys.getcwd () in
  Lwt_unix.chdir path >>= fun () ->
  Lwt.finalize f (fun () -> Lwt_unix.chdir old_cwd)

let file_default_perm = 0o600

let directory_default_perm = 0o700

let open_file path size =
  let rec loop path =
    match path with
    | [] -> assert false
    | [ path ] ->
        Lwt_unix.openfile path [ Unix.O_CREAT; Unix.O_RDWR ] file_default_perm
        >>= fun fd ->
        Lwt_unix.LargeFile.ftruncate fd size >>= fun () -> Lwt.return fd
    | dir :: path ->
        if Sys.file_exists dir then cd dir (fun () -> loop path)
        else
          Logs_lwt.debug (fun m -> m "creating directory %S" dir) >>= fun () ->
          Lwt_unix.mkdir dir directory_default_perm >>= fun () ->
          cd dir (fun () -> loop path)
  in
  loop path

let close files =
  Lwt_list.iter_p
    (fun (fd, _, m) -> Lwt_mutex.with_lock m (fun () -> Lwt_unix.close fd))
    files

let create files =
  let open_file (path, size) =
    open_file path size >>= fun fd -> Lwt.return (fd, size, Lwt_mutex.create ())
  in
  Lwt_list.map_s open_file files
