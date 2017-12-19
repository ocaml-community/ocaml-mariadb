open Printf

module S = Mariadb.Nonblocking.Status

let env var def = try Sys.getenv var with Not_found -> def
let host = env "OCAML_MARIADB_HOST" "localhost"
let user = env "OCAML_MARIADB_USER" "root"
let pass = env "OCAML_MARIADB_PASS" ""
let db   = env "OCAML_MARIADB_DB" "mysql"

module Make (W : Mariadb.Nonblocking.Wait) = struct
  module M = Mariadb.Nonblocking.Make (W)
  open W.IO

  let or_die where = function
    | Ok r -> return r
    | Error (i, e) -> eprintf "%s: (%d) %s\n%!" where i e; exit 2

  let connect () = M.connect ~host ~user ~pass ~db ()

  let rec repeat n f =
    printf "> %d\n%!" n;
    if n = 0 then return () else f () >>= fun () -> repeat (n - 1) f

  let test () =
    connect () >>= or_die "connect" >>= fun dbh ->
    M.close dbh

  let main () = repeat 50000 test
end

module Test = Make (struct
  module IO = struct
    type 'a future = 'a
    let (>>=) x f = f x
    let return x = x
  end

  let return = IO.return

  let wait mariadb status =
    let fd = Mariadb.Nonblocking.fd mariadb in
    let rfd = if S.read status then [fd] else [] in
    let wfd = if S.write status then [fd] else [] in
    let efd = if S.except status then [fd] else [] in
    let timeout =
      if S.timeout status
      then float @@ Mariadb.Nonblocking.timeout mariadb
      else -1.0 in
    try
      let rfd, wfd, efd = Unix.select rfd wfd efd timeout in
      return @@
        S.create
          ~read:(rfd <> [])
          ~write:(wfd <> [])
          ~except:(efd <> [])
          ()
    with Unix.Unix_error (e, _, _) ->
      return @@ S.create ~timeout: true ()
end)

let () = Test.main ()
