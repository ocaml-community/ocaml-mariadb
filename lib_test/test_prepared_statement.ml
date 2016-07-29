let int_of_file_descr : Unix.file_descr -> int = Obj.magic
let file_descr_of_int : int -> Unix.file_descr = Obj.magic

let die where err =
  failwith @@ Printf.sprintf "%s: (%d) %s\n%!"
    where (Mariadb.Error.errno err) (Mariadb.Error.message err)

let wait mariadb status =
  let fd = file_descr_of_int @@ Mariadb.Nonblocking.fd mariadb in
  let rfd = if Mariadb.Nonblocking.Status.read status then [fd] else [] in
  let wfd = if Mariadb.Nonblocking.Status.write status then [fd] else [] in
  let efd = if Mariadb.Nonblocking.Status.except status then [fd] else [] in
  let timeout =
    if Mariadb.Nonblocking.Status.timeout status
    then float @@ Mariadb.Nonblocking.timeout mariadb
    else -1.0 in
  try
    let rfd, wfd, efd = Unix.select rfd wfd efd timeout in
    let status =
      Mariadb.Nonblocking.Status.create
        ~read:(rfd <> [])
        ~write:(wfd <> [])
        ~except:(efd <> [])
        () in
    status
  with Unix.Unix_error (e, _, _) ->
    Mariadb.Nonblocking.Status.create ~timeout: true ()

let rec handle_connect mariadb f =
  match f mariadb with
  | `Ok m -> m
  | `Wait status -> connect_cont mariadb status
  | `Error err -> die __LOC__ err

and connect_cont mariadb status =
  print_endline "waiting for connection...";
  let status = wait mariadb status in
  handle_connect
    mariadb (fun m -> Mariadb.Nonblocking.connect_cont mariadb status)

let env var def =
  try Sys.getenv var
  with Not_found -> def

let connect_start mariadb =
  handle_connect mariadb
    (fun m ->
      Mariadb.Nonblocking.connect_start m
        ~host:(env "OCAML_MARIADB_HOST" "localhost")
        ~user:(env "OCAML_MARIADB_USER" "root")
        ~pass:(env "OCAML_MARIADB_PASS" "")
        ~db:(env "OCAML_MARIADB_DB" "mysql")
        ())

let bind_params stmt args =
  match Mariadb.Stmt.bind_params stmt args with
  | `Ok bound -> bound
  | `Error err ->
      let errno = Mariadb.Stmt.Error.errno err in
      failwith @@ "bind_params: " ^ string_of_int errno

let rec handle_stmt_close stmt f =
  match f stmt with
  | `Ok () -> ()
  | `Wait status -> stmt_close_cont stmt status
  | `Error err ->
      failwith @@ "stmt_close: " ^ string_of_int @@ Mariadb.Stmt.Error.errno err

and stmt_close_cont stmt status =
  handle_stmt_close stmt
    (fun stmt -> Mariadb.Nonblocking.Stmt.close_cont stmt status)

and stmt_close_start stmt =
  handle_stmt_close stmt
    (fun stmt -> Mariadb.Nonblocking.Stmt.close_start stmt)

let rec nonblocking ~name (f, g) =
  match f () with
  | `Ok v -> v
  | `Wait s -> nonblocking ~name ((fun () -> g s), g)
  | `Error e -> failwith @@ name ^ " failed"

let prepare mariadb query =
  match Mariadb.Nonblocking.prepare mariadb query with
  | `Ok nb -> nonblocking ~name:"nonblocking prepare" nb
  | `Error e -> failwith "prepare failed"

let execute stmt params =
  match Mariadb.Nonblocking.Stmt.execute stmt params with
  | `Ok nb -> nonblocking ~name:"nonblocking execute" nb
  | `Error e -> failwith "execute failed"

let store_result stmt =
  nonblocking ~name:"store result" (Mariadb.Nonblocking.Stmt.store_result stmt)

let () =
  let mariadb =
    match Mariadb.Nonblocking.init () with
    | Some m -> connect_start m
    | None -> failwith "cannot init" in
  print_endline "connected!";
  let query =
    env "OCAML_MARIADB_QUERY" "SELECT * FROM user WHERE LENGTH(user) > ?" in
  let stmt = prepare mariadb query in
  let stmt = execute stmt [| `String "Problema%" |] in
  let res = store_result stmt in
  print_endline @@ string_of_int @@ Mariadb.Res.num_rows res;
  stmt_close_start stmt;
  print_endline "todo - results"
