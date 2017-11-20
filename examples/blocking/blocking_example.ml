open Printf

module M = Mariadb.Blocking

let env var def =
  try Sys.getenv var
  with Not_found -> def

let or_die where = function
  | Ok r -> r
  | Error (i, e) -> failwith @@ sprintf "%s: (%d) %s" where i e

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
    match M.Res.fetch (module M.Row.Map) res with
    | Ok (Some _ as row) -> row
    | Ok None -> None
    | Error e -> raise (F.E e) in
  try Ok (Stream.from next)
  with F.E e -> Error e

let main () =
  let mariadb = connect () |> or_die "connect" in
  let query = env "OCAML_MARIADB_QUERY"
    "SELECT * FROM mysql.user WHERE User LIKE ?" in
  let stmt = M.prepare mariadb query |> or_die "prepare" in
  let res = M.Stmt.execute stmt [| `String "r%" |] |> or_die "exec" in
  begin match res with
  | Some res ->
    printf "#rows: %d\n%!" (M.Res.num_rows res);
    let s = stream res |> or_die "stream" in
    Stream.iter print_row s
  | None -> ()
  end;
  M.Stmt.close stmt |> or_die "stmt close";
  M.close mariadb;
  M.library_end ();
  printf "done\n%!"

let () = main ()
