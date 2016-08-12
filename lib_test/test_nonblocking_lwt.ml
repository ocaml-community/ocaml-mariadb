open Lwt.Infix
open Printf

module S = Mariadb.Nonblocking.Status
module M = Mariadb.Nonblocking.Make(struct
  type 'a future = 'a Lwt.t
  let (>>=) = (>>=)
  let return = Lwt.return

  let wait mariadb status =
    let fd = Lwt_unix.of_unix_file_descr @@ Mariadb.Nonblocking.fd mariadb in
    let rt =
      if S.read status then Lwt_unix.wait_read fd
      else Lwt.return_unit in
    let wt =
      if S.write status then Lwt_unix.wait_write fd
      else Lwt.return_unit in
    (*let tt =
      let tmout = float (Mariadb.Nonblocking.timeout mariadb) in
      if S.timeout status then Lwt_unix.timeout tmout
      else Lwt.return_unit in*)
    Lwt.catch
      (fun () ->
        (* XXX shouldn't it be Lwt.nchoose here? *)
        Lwt.join [rt; wt] >>= fun _ ->
        return @@
          S.create
            ~read:(Lwt_unix.readable fd)
            ~write:(Lwt_unix.writable fd)
            ())
      (function
      | Lwt_unix.Timeout -> return @@ S.create ~timeout:true ()
      | e -> Lwt.fail e)
end)

let env var def =
  try Sys.getenv var
  with Not_found -> def

let or_die ?(info = "error") () = function
  | Ok r -> Lwt.return r
  | Error (i, e) -> Lwt.fail_with @@ sprintf "%s: (%d) %s" info i e

let print_row row =
  printf "---\n%!";
  M.Row.StringMap.iter
    (fun name field ->
      printf "%20s " name;
      match M.Field.value field with
      | `Int i -> printf "%d\n%!" i
      | `Float x -> printf "%f\n%!" x
      | `String s -> printf "%s\n%!" s
      | `Bytes b -> printf "%s\n%!" (Bytes.to_string b)
      | `Time t ->
          printf "%04d-%02d-%02d %02d:%02d:%02d\n%!"
            (M.Time.year t)
            (M.Time.month t)
            (M.Time.day t)
            (M.Time.hour t)
            (M.Time.minute t)
            (M.Time.second t)
      | `Null -> printf "NULL\n%!")
    row

let connect () =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~pass:(env "OCAML_MARIADB_PASS" "")
    ~db:(env "OCAML_MARIADB_DB" "mysql") ()

let stream res =
  let module F = struct exception E of M.error end in
  let next _ =
    M.Res.fetch (module M.Row.Map) res
    >>= function
    | Ok (Some _ as row) -> Lwt.return row
    | Ok None -> Lwt.return_none
    | Error e -> Lwt.fail (F.E e) in
  Lwt.catch
    (fun () -> Lwt.return (Ok (Lwt_stream.from next)))
    (function F.E e -> Lwt.return (Error e))

let main () =
  connect () >>= or_die ~info:"connect" () >>= fun mariadb ->
  let query = env "OCAML_MARIADB_QUERY"
    "SELECT * FROM user WHERE LENGTH(user) > ?" in
  M.prepare mariadb query >>= or_die ~info:"prepare" () >>= fun stmt ->
  M.Stmt.execute stmt [| `String "Problema%" |] >>= or_die () >>= fun res ->
  printf "#rows: %d\n%!" (M.Res.num_rows res);
  stream res >>= or_die () >>= fun s ->
  Lwt_stream.iter print_row s >>= fun () ->
  M.Stmt.close stmt >>= or_die () >>= fun () ->
  M.close mariadb

let () =
  Lwt_main.run @@ main ()
