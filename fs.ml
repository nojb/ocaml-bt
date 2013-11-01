let rec project_file_descrs ~file_descrs ~offset ~size =
  if size < 0 then failwith "project_file_descrs: negative size"
  else match file_descrs with
  | [] -> failwith "projecti_file_descrs: tring to read beyond torrent length"
  | (fd, size1) :: fds ->
      if offset >= size1 then project_file_descrs fds (offset - size1) size
      else begin
        let size1 = size1 - offset in
        if size1 >= size then [(fd, offset, size)]
        else
          [fd, offset, size1] :: project_file_descrs fds 0 (size-size1)
      end

let really_read len fd =
  let buf = String.create len in
  let rec loop rem off =
    Lwt_unix.read fd buf off rem >>= fun actually_read ->
    if actually_read = 0 && rem > 0 then failwith "end of file"
    else if actually_read < rem then loop (rem - actually_read) (off + actually_read)
    else Lwt.return buf
  in loop len 0

let check_piece handles pi : bool Lwt.t =
  let chunks = project_file_descrs handles pi.piece_offset pi.piece_length in
  lwt chunks = Lwt_list.map_s (fun (fd, off, size) -> (* FIXME: Lwt_map *)
    Lwt_unix.LargeFile.lseek fd off Lwt_unix.SEEK_SET >>= fun _ ->
    let buf = String.create size in
    really_read fd buf ofs size >>= fun _ ->
    return buf) chunks in
    (* Lwt_unix.read fd buf ofs size; (* FIXME really_read *) *)
    (* buf) chunks in *)
  let digest = Sha1.string (String.concat "" chunks) in
  return (Digest.equal digest pi.piece_digest)

let check_file handles pieces : Bits.t Lwt.t =
  let have = Bits.zero (Array.length pieces) in
  let rec loop have i =
    if i >= Array.length pieces then Lwt.return have
    else
      check_piece handles pieces.(i) >>= fun valid ->
      if valid then
        loop (Bits.set i have) (i+1)
      else
        loop have (i+1)
  in loop have 0

let open_and_check_file info : ((Lwt_unix.file_descr * int64) list * Bits.t) Lwt.t =
  lwt handles = Lwt_list.map_s (fun fi ->
    let path = String.concat "/" fi.Torrent.file_path in
    (* FIXME *)
    lwt fd = Lwt_unix.openfile path [Unix.O_RDRW; Unix.O_NONBLOCK; Unix.O_CREAT] 644 in
    Lwt.return (fd, fi.Torrent.file_size)) info.Torrent.files in
  check_file handles info.Torrent.pieces >>= fun have ->
  Lwt.return (handles, have)
