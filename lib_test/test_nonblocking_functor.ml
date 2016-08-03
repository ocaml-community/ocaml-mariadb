module S = Mariadb.Nonblocking.Status
module M = Mariadb.Nonblocking.Make(struct
  let file_descr_of_int : int -> Unix.file_descr = Obj.magic

  let wait mariadb status =
    let fd = file_descr_of_int @@ Mariadb.Nonblocking.fd mariadb in
    let rfd = if S.read status then [fd] else [] in
    let wfd = if S.write status then [fd] else [] in
    let efd = if S.except status then [fd] else [] in
    let timeout =
      if S.timeout status
      then float @@ Mariadb.Nonblocking.timeout mariadb
      else -1.0 in
    try
      let rfd, wfd, efd = Unix.select rfd wfd efd timeout in
      let status =
        S.create
          ~read:(rfd <> [])
          ~write:(wfd <> [])
          ~except:(efd <> [])
          () in
      status
    with Unix.Unix_error (e, _, _) ->
      S.create ~timeout: true ()
end)

module T = Test_common.Make (M)
let () = T.main ()
