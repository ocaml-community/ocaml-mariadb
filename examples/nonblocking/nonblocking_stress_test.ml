open Printf

module Make (W : Mariadb.Nonblocking.Wait) = struct
  module M = Mariadb.Nonblocking.Make (W)
  open W.IO

  let (>|=) m f = m >>= fun x -> return (f x)

  let env var def = try Sys.getenv var with Not_found -> def

  let or_die where = function
    | Ok r -> return r
    | Error (i, e) -> eprintf "%s: (%d) %s\n%!" where i e; exit 2

  let connect () =
    M.connect
      ~host:(env "OCAML_MARIADB_HOST" "localhost")
      ~user:(env "OCAML_MARIADB_USER" "root")
      ~pass:(env "OCAML_MARIADB_PASS" "")
      ~db:(env "OCAML_MARIADB_DB" "mysql") ()

  let rec repeat n f =
    if n = 0 then return () else f () >>= fun () -> repeat (n - 1) f

  let test () =
    connect () >>= or_die "connect" >>= fun dbh ->
    let mk_stmt () =  M.prepare dbh "SELECT ?" >>= or_die "prepare" in
    mk_stmt () >>= fun init_stmt ->
    let stmt = ref init_stmt in
    repeat 100 begin fun () ->
      let n = Random.int (1 lsl Random.int 8) in
      let s = String.init n (fun i -> "ACGT".[Random.int 4]) in
      M.Stmt.execute !stmt [|`String s|] >>= or_die "execute" >>= fun res ->
      assert (M.Res.num_rows res = 1);
      M.Res.fetch (module M.Row.Array) res >>= or_die "fetch" >>=
      (function
       | None -> assert false
       | Some row ->
           let s' = M.Field.string row.(0) in
           if s <> s' then eprintf "@@@ <%s> <%s>\n%!" s s';
           assert (s = s');
           return ()) >>= fun () ->
      if Random.bool () then
        M.Stmt.close !stmt >>= or_die "close" >>= fun () ->
        mk_stmt () >|= fun next_stmt ->
        stmt := next_stmt
      else
        M.Stmt.reset !stmt >>= or_die "reset"
    end >>= fun () ->
    M.close dbh

  let main () = repeat 500 test
end
