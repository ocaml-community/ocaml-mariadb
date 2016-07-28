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

let rec handle_query mariadb f =
  match f mariadb with
  | `Ok () -> ()
  | `Wait status -> query_cont mariadb status
  | `Error err -> die __LOC__ err

and query_cont mariadb status =
  print_endline "waiting for query...";
  let status = wait mariadb status in
  handle_query mariadb (fun m -> Mariadb.Nonblocking.query_cont m status)

let query_start mariadb query =
  handle_query mariadb (fun m -> Mariadb.Nonblocking.query_start m query)

let print_row r =
  print_endline "[";
  for i = 0 to Array.length r - 1 do
    print_endline @@ "  (" ^ r.(i) ^ ")"
  done;
  print_endline "]"

let rec fetch_row_cont mariadb res status =
  print_endline "waiting for row...";
  let status = wait mariadb status in
  match Mariadb.Nonblocking.Res.fetch_row_cont res status with
  | `Ok row -> print_row row; fetch_row_start mariadb res
  | `Wait status -> fetch_row_cont mariadb res status
  | `Done ->
      let err = Mariadb.Error.create mariadb in
      if Mariadb.Error.errno err <> 0 then
        print_endline @@ "fetch_row_cont: " ^ Mariadb.Error.message err

and fetch_row_start mariadb res =
  match Mariadb.Nonblocking.Res.fetch_row_start res with
  | `Ok row -> print_row row; fetch_row_start mariadb res
  | `Wait status -> fetch_row_cont mariadb res status
  | `Done ->
      let err = Mariadb.Error.create mariadb in
      if Mariadb.Error.errno err <> 0 then
        print_endline @@ "fetch_row_start: " ^ Mariadb.Error.message err

let rec handle_prepare stmt f =
  match f stmt with
  | `Ok prep -> prep
  | `Wait status -> prepare_cont stmt status
  | `Error err -> failwith @@ "prepare: " ^ Mariadb.Error.message err

and prepare_cont stmt status =
  handle_prepare stmt
    (fun stmt -> Mariadb.Nonblocking.Stmt.prepare_cont stmt status)

and prepare_start stmt query =
  handle_prepare stmt
    (fun stmt -> Mariadb.Nonblocking.Stmt.prepare_start stmt query)

let rec handle_execute stmt f =
  match f stmt with
  | `Ok execed -> execed
  | `Wait status -> execute_cont stmt status
  | `Error err ->
      failwith @@ "execute: (" ^ (string_of_int @@ Mariadb.Error.errno err) ^
                  ") :" ^ Mariadb.Error.message err

and execute_cont stmt status =
  handle_execute stmt
    (fun stmt -> Mariadb.Nonblocking.Stmt.execute_cont stmt status)

and execute_start stmt =
  handle_execute stmt
    (fun stmt -> Mariadb.Nonblocking.Stmt.execute_start stmt)

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
      failwith @@ "stmt_close: " ^ string_of_int @@ Mariadb.Error.errno err

and stmt_close_cont stmt status =
  handle_stmt_close stmt
    (fun stmt -> Mariadb.Nonblocking.Stmt.close_cont stmt status)

and stmt_close_start stmt =
  handle_stmt_close stmt
    (fun stmt -> Mariadb.Nonblocking.Stmt.close_start stmt)

let () =
  let mariadb =
    match Mariadb.Nonblocking.init () with
    | Some m -> connect_start m
    | None -> failwith "cannot init" in
  print_endline "connected!";
  let query =
    env "OCAML_MARIADB_QUERY" "SELECT * FROM user WHERE LENGTH(user) > ?" in
  let stmt =
    match Mariadb.Stmt.init mariadb with
    | Some stmt -> prepare_start stmt query
    | None -> failwith "cannot prepare" in
  let stmt = bind_params stmt [| `String "Problema%" |] in
  let stmt = execute_start stmt in
  stmt_close_start stmt;
  print_endline "todo - results"
