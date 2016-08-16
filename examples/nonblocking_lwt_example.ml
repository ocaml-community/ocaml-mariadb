open Lwt.Infix
open Printf

module S = Mariadb.Nonblocking.Status
module M = Mariadb.Nonblocking.Make(struct
  module IO = struct
    type 'a future = 'a Lwt.t
    let (>>=) = (>>=)
    let return = Lwt.return
  end

  let wait mariadb status =
    let fd = Lwt_unix.of_unix_file_descr @@ Mariadb.Nonblocking.fd mariadb in
    assert (S.read status || S.write status || S.timeout status);
    let idle, _ = Lwt.task () in
    let rt =
      if S.read status then Lwt_unix.wait_read fd
      else idle in
    let wt =
      if S.write status then Lwt_unix.wait_write fd
      else idle in
    let tt =
      let tmout = float (Mariadb.Nonblocking.timeout mariadb) in
      if S.timeout status then Lwt_unix.timeout tmout
      else idle in
    Lwt.catch
      (fun () ->
        Lwt.nchoose [rt; wt; tt] >>= fun _ ->
        Lwt.return @@
          S.create
            ~read:(Lwt_unix.readable fd)
            ~write:(Lwt_unix.writable fd)
            ())
      (function
      | Lwt_unix.Timeout -> Lwt.return @@ S.create ~timeout:true ()
      | e -> Lwt.fail e)
end)

let env var def =
  try Sys.getenv var
  with Not_found -> def

let or_die where = function
  | Ok r -> Lwt.return r
  | Error (i, e) -> Lwt.fail_with @@ sprintf "%s: (%d) %s" where i e

let print_row row =
  Lwt_io.printf "---\n%!" >>= fun () ->
  M.Row.StringMap.fold
    (fun name field _ ->
      Lwt_io.printf "%20s " name >>= fun () ->
      match M.Field.value field with
      | `Int i -> Lwt_io.printf "%d\n%!" i
      | `Float x -> Lwt_io.printf "%f\n%!" x
      | `String s -> Lwt_io.printf "%s\n%!" s
      | `Bytes b -> Lwt_io.printf "%s\n%!" (Bytes.to_string b)
      | `Time t ->
          Lwt_io.printf "%04d-%02d-%02d %02d:%02d:%02d\n%!"
            (M.Time.year t)
            (M.Time.month t)
            (M.Time.day t)
            (M.Time.hour t)
            (M.Time.minute t)
            (M.Time.second t)
      | `Null -> Lwt_io.printf "NULL\n%!")
    row
    Lwt.return_unit

let connect () =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~pass:(env "OCAML_MARIADB_PASS" "")
    ~db:(env "OCAML_MARIADB_DB" "mysql") ()

let stream res =
  let next _ =
    M.Res.fetch (module M.Row.Map) res
    >>= function
    | Ok (Some _ as row) -> Lwt.return row
    | Ok None -> Lwt.return_none
    | Error _ -> Lwt.return_none in
  Lwt.return (Lwt_stream.from next)

let main () =
  connect () >>= or_die "connect" >>= fun mariadb ->
  let query = env "OCAML_MARIADB_QUERY"
    "SELECT * FROM mysql.user WHERE User LIKE ?" in
  M.prepare mariadb query >>= or_die "prepare" >>= fun stmt ->
  M.Stmt.execute stmt [| `String "r%" |] >>= or_die "exec" >>= fun res ->
  Lwt_io.printf "#rows: %d\n%!" (M.Res.num_rows res) >>= fun () ->
  stream res >>= fun s ->
  Lwt_stream.iter_s print_row s >>= fun () ->
  M.Stmt.close stmt >>= or_die "stmt close" >>= fun () ->
  M.close mariadb

let () =
  Lwt_main.run @@ main ()
