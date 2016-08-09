open Printf

module M = Mariadb.Nonblocking

let file_descr_of_int : int -> Unix.file_descr = Obj.magic

let wait mariadb status =
  let fd = file_descr_of_int @@ M.fd mariadb in
  let rfd = if M.Status.read status then [fd] else [] in
  let wfd = if M.Status.write status then [fd] else [] in
  let efd = if M.Status.except status then [fd] else [] in
  let timeout =
    if M.Status.timeout status
    then float @@ M.timeout mariadb
    else -1.0 in
  try
    let rfd, wfd, efd = Unix.select rfd wfd efd timeout in
    let status =
      M.Status.create
        ~read:(rfd <> [])
        ~write:(wfd <> [])
        ~except:(efd <> [])
        () in
    status
  with Unix.Unix_error (e, _, _) ->
    M.Status.create ~timeout: true ()

let env var def =
  try Sys.getenv var
  with Not_found -> def

let rec nonblocking mariadb ~name (f, g) =
  match f () with
  | `Ok v -> v
  | `Wait s -> nonblocking mariadb ~name ((fun () -> g (wait mariadb s)), g)
  | `Error (errno, msg) -> failwith @@ name ^ " failed: " ^ msg

let connect mariadb =
  nonblocking mariadb ~name:"connect"
    (M.connect mariadb
      ~host:(env "OCAML_MARIADB_HOST" "localhost")
      ~user:(env "OCAML_MARIADB_USER" "root")
      ~pass:(env "OCAML_MARIADB_PASS" "")
      ~db:(env "OCAML_MARIADB_DB" "mysql") ())

let prepare mariadb query =
  match M.prepare mariadb query with
  | `Ok nb -> nonblocking mariadb ~name:"nonblocking prepare" nb
  | `Error e -> failwith "prepare failed"

let execute mariadb stmt params =
  match M.Stmt.execute stmt params with
  | `Ok nb -> nonblocking mariadb ~name:"nonblocking execute" nb
  | `Error e -> failwith "execute failed"

let store_result mariadb stmt =
  nonblocking mariadb ~name:"store result"
    (M.Stmt.store_result stmt)

let fetch mariadb res =
  nonblocking mariadb ~name:"fetch" (M.Res.fetch (module Mariadb.Row.Array) res)

let close_stmt mariadb stmt =
  nonblocking mariadb ~name:"close result" (M.Stmt.close stmt)

let rec each_result mariadb res f =
  match fetch mariadb res with
  | Some row -> f row; each_result mariadb res f
  | None -> ()

let print_row row =
  printf "---\n%!";
  Array.iter
    (fun field ->
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

let () =
  let mariadb =
    match M.init () with
    | Some m -> connect m
    | None -> failwith "cannot init" in
  let query = env "OCAML_MARIADB_QUERY"
    "SELECT * FROM user WHERE LENGTH(user) > ?" in
  let stmt = prepare mariadb query in
  let stmt = execute mariadb stmt [| `String "Problema%" |] in
  let res = store_result mariadb stmt in
  printf "#rows: %d\n%!" (M.Res.num_rows res);
  each_result mariadb res print_row;
  close_stmt mariadb stmt;
  printf "done\n%!"
