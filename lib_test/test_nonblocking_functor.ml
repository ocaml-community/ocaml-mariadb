open Printf

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

let env var def =
  try Sys.getenv var
  with Not_found -> def

let or_die = function
  | Ok r -> r
  | Error (i, e) -> failwith @@ "error " ^ string_of_int i ^ ": " ^ e

let rec each_result res f =
  match M.Res.fetch res |> or_die with
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

let connect mariadb =
  M.connect mariadb
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~pass:(env "OCAML_MARIADB_PASS" "")
    ~db:(env "OCAML_MARIADB_DB" "mysql") () |> or_die

let () =
  let mariadb =
    match M.init () with
    | Some m -> connect m
    | None -> failwith "cannot init" in
  let query =
    env "OCAML_MARIADB_QUERY" "SELECT * FROM user WHERE LENGTH(user) > ?" in
  let stmt = M.prepare mariadb query |> or_die in
  let res = M.Stmt.execute stmt [| `Int 5 |] |> or_die in
  printf "#rows: %d\n%!" (M.Res.num_rows res);
  each_result res print_row;
  M.Res.free res;
  M.Stmt.close stmt |> or_die;
  M.close mariadb;
  printf "done\n%!"
