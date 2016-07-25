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
  | `Ok () -> mariadb
  | `Wait status -> connect_cont mariadb status
  | `Error err -> die __LOC__ err

and connect_cont mariadb status =
  print_endline "waiting for connection...";
  let status = wait mariadb status in
  handle_connect mariadb (fun m -> Mariadb.Nonblocking.connect_cont m status)

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
  match Mariadb.Nonblocking.fetch_row_cont res status with
  | `Ok row -> print_row row; fetch_row_start mariadb res
  | `Wait status -> fetch_row_cont mariadb res status
  | `Done ->
      let err = Mariadb.Error.create mariadb in
      if Mariadb.Error.errno err <> 0 then
        print_endline @@ "fetch_row_cont: " ^ Mariadb.Error.message err

and fetch_row_start mariadb res =
  match Mariadb.Nonblocking.fetch_row_start res with
  | `Ok row -> print_row row; fetch_row_start mariadb res
  | `Wait status -> fetch_row_cont mariadb res status
  | `Done ->
      let err = Mariadb.Error.create mariadb in
      if Mariadb.Error.errno err <> 0 then
        print_endline @@ "fetch_row_start: " ^ Mariadb.Error.message err

let () =
  let mariadb =
    match Mariadb.Nonblocking.init () with
    | Some m -> connect_start m
    | None -> failwith "cannot init" in
  print_endline "connected!";
  let query = env "OCAML_MARIADB_QUERY" "SELECT * FROM user LIMIT 10" in
  query_start mariadb query;
  match Mariadb.use_result mariadb with
  | Some res ->
      fetch_row_start mariadb res;
      print_endline "freeing result";
      Mariadb.Res.free res;
      print_endline "closing";
      Mariadb.close(mariadb)
  | None ->
      let err = Mariadb.Error.create mariadb in
      print_endline @@ "no res: " ^ Mariadb.Error.message err
