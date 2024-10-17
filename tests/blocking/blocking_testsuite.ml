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

let execute_no_data stmt =
  let res = M.Stmt.execute stmt [||] |> or_die "execute" in
  assert (M.Res.num_rows res = 0)

let fetch_single_row res =
  assert (M.Res.num_rows res = 1);
  let row = M.Res.fetch (module M.Row.Array) res |> or_die "fetch" in
  (match row with
   | None -> failwith "expecting one row, no rows returned"
   | Some a -> a)

let test_txn () =
  let dbh = connect () |> or_die "connect" in

  let create_table_stmt =
    M.prepare dbh
      "CREATE TEMPORARY TABLE ocaml_mariadb_test (i integer PRIMARY KEY)"
    |> or_die "prepare create_table_stmt"
  in
  execute_no_data create_table_stmt;

  let insert_stmts =
    List.map (fun s -> M.prepare dbh s |> or_die "prepare insert") [
      "INSERT INTO ocaml_mariadb_test VALUES (1), (2)";
      "INSERT INTO ocaml_mariadb_test SELECT i + 10 FROM ocaml_mariadb_test";
    ]
  in
  let sum_stmt =
    M.prepare dbh "SELECT CAST(sum(i) AS integer) FROM ocaml_mariadb_test"
    |> or_die "prepare sum"
  in

  M.start_txn dbh |> or_die "start_txn";
  List.iter execute_no_data insert_stmts;
  M.rollback dbh |> or_die "rollback";
  let res = M.Stmt.execute sum_stmt [||] |> or_die "execute" in
  let row = fetch_single_row res in
  assert (Array.length row = 1 && M.Field.null_value row.(0));

  M.start_txn dbh |> or_die "start_txn";
  List.iter execute_no_data insert_stmts;
  M.commit dbh |> or_die "rollback";
  let res = M.Stmt.execute sum_stmt [||] |> or_die "execute" in
  let row = fetch_single_row res in
  assert (Array.length row = 1 && M.Field.int row.(0) = 26);

  M.close dbh

let test_random_select () =
  let dbh = connect () |> or_die "connect" in
  (* without CAST result is typed as NULL for some reason *)
  let mk_stmt () =  M.prepare dbh "SELECT CAST(? AS BINARY)" |> or_die "prepare" in
  let stmt = ref (mk_stmt ()) in
  for _ = 1 to 100 do
    let n = Random.int (1 lsl Random.int 8) in
    let s = String.init n (fun _ -> "ACGT".[Random.int 4]) in
    let res = M.Stmt.execute !stmt [|`String s|] |> or_die "Stmt.execute" in
    assert (M.Res.num_rows res = 1);
    (match M.Res.fetch (module M.Row.Array) res |> or_die "Res.fetch" with
     | None -> assert false
     | Some row ->
         let s' = M.Field.string row.(0) in
         if s <> s' then printf "@@@ <%s> <%s>\n%!" s s';
         assert (s = s'));
    if Random.bool () then begin
      M.Stmt.close !stmt |> or_die "Stmt.close";
      stmt := mk_stmt ()
    end else
      M.Stmt.reset !stmt |> or_die "Stmt.reset"
  done;
  M.Stmt.close !stmt |> or_die "Stmt.close";
  M.close dbh

let test_many_select () = for _ = 1 to 500 do test_random_select () done

let () =
  test_txn ();
  test_many_select ()
