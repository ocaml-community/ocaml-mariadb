open Printf

module M = Mariadb.Blocking

let env var def =
  try Sys.getenv var
  with Not_found -> def

let or_die where = function
  | Ok r -> r
  | Error (i, e) -> ksprintf failwith "%s: (%d) %s" where i e

let connect () =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~pass:(env "OCAML_MARIADB_PASS" "")
    ~db:(env "OCAML_MARIADB_DB" "mysql") ()

let test dbh =
  let mk_stmt () =  M.prepare dbh "SELECT ?" |> or_die "prepare" in
  let stmt = ref (mk_stmt ()) in
  for i = 0 to 500000 do
    let n = Random.int (1 lsl Random.int 8) in
    let s = String.init n (fun i -> "ACGT".[Random.int 4]) in
    (match M.Stmt.execute !stmt [|`String s|] |> or_die "execute" with
     | Some res ->
        assert (M.Res.num_rows res = 1);
        (match M.Res.fetch (module M.Row.Array) res |> or_die "fetch" with
         | None -> assert false
         | Some row ->
             let s' = M.Field.string row.(0) in
             if s <> s' then printf "@@@ <%s> <%s>\n%!" s s';
             assert (s = s'))
     | None -> assert false);
    if Random.bool () then begin
      M.Stmt.close !stmt |> or_die "close";
      stmt := mk_stmt ()
    end else
      M.Stmt.reset !stmt |> or_die "reset"
  done

let () = test (connect () |> or_die "connect")
