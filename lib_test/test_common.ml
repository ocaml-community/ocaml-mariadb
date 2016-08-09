open Printf

module Make (M : Mariadb.S) = struct
  let env var def =
    try Sys.getenv var
    with Not_found -> def

  let or_die ?(info = "error") () = function
    | Ok r -> r
    | Error (i, e) -> failwith @@ sprintf "%s: (%d) %s" info i e

  let rec each_result res f =
    match M.Res.fetch res |> or_die () with
    | Some row -> f row; each_result res f
    | None -> ()

  let print_row row =
    printf "---\n%!";
    Array.iter
      (function
      | `Int i -> printf "%d\n%!" i
      | `Float x -> printf "%f\n%!" x
      | `String s -> printf "%s\n%!" s
      | `Bytes b -> printf "%s\n%!" (Bytes.to_string b)
      | `Time t -> printf "%04d-%02d-%02d %02d:%02d:%02d\n%!"
            (t.M.Res.year)
            (t.M.Res.month)
            (t.M.Res.day)
            (t.M.Res.hour)
            (t.M.Res.minute)
            (t.M.Res.second)
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
    each_result res print_row;
    M.Stmt.close stmt |> or_die ();
    M.close mariadb;
    printf "done\n%!"
end
