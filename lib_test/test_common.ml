open Printf

module Make (M : Mariadb.S) = struct
  let env var def =
    try Sys.getenv var
    with Not_found -> def

  let or_die ?(info = "error") () = function
    | Ok r -> r
    | Error (i, e) -> failwith @@ sprintf "%s: (%d) %s" info i e

  let print_row row =
    printf "---\n%!";
    Mariadb.StringMap.iter
      (fun name field ->
        printf "%20s " name;
        match Mariadb.Field.value field with
        | `Int i -> printf "%d\n%!" i
        | `Float x -> printf "%f\n%!" x
        | `String s -> printf "%s\n%!" s
        | `Bytes b -> printf "%s\n%!" (Bytes.to_string b)
        | `Time t -> printf "%04d-%02d-%02d %02d:%02d:%02d\n%!"
              (t.Mariadb.Field.year)
              (t.Mariadb.Field.month)
              (t.Mariadb.Field.day)
              (t.Mariadb.Field.hour)
              (t.Mariadb.Field.minute)
              (t.Mariadb.Field.second)
        | `Null -> printf "NULL\n%!")
      row

  let connect () =
    M.connect
      ~host:(env "OCAML_MARIADB_HOST" "localhost")
      ~user:(env "OCAML_MARIADB_USER" "root")
      ~pass:(env "OCAML_MARIADB_PASS" "")
      ~db:(env "OCAML_MARIADB_DB" "mysql") ()

  let main () =
    let mariadb = connect () |> or_die ~info:"connect" () in
    let query = env "OCAML_MARIADB_QUERY"
      "SELECT * FROM user WHERE LENGTH(user) > ?" in
    let stmt = M.prepare mariadb query |> or_die ~info:"prepare" () in
    let res = M.Stmt.execute stmt [| `Int 5 |] |> or_die () in
    printf "#rows: %d\n%!" (M.Res.num_rows res);
    let stream = M.Res.stream (module Mariadb.Row.Map) res |> or_die () in
    Stream.iter print_row stream;
    M.Stmt.close stmt |> or_die ();
    M.close mariadb;
    printf "done\n%!"
end
