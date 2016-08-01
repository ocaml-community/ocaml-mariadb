open Printf

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

let env var def =
  try Sys.getenv var
  with Not_found -> def

let bind_params stmt args =
  match Mariadb.Stmt.bind_params stmt args with
  | `Ok bound -> bound
  | `Error err ->
      let errno = Mariadb.Stmt.Error.errno err in
      failwith @@ "bind_params: " ^ string_of_int errno

let rec nonblocking mariadb ~name (f, g) =
  match f () with
  | `Ok v -> v
  | `Wait s -> nonblocking mariadb ~name ((fun () -> g (wait mariadb s)), g)
  | `Error (errno, msg) -> failwith @@ name ^ " failed: " ^ msg

let connect mariadb =
  nonblocking mariadb ~name:"connect"
    (Mariadb.Nonblocking.connect mariadb
      ~host:(env "OCAML_MARIADB_HOST" "localhost")
      ~user:(env "OCAML_MARIADB_USER" "root")
      ~pass:(env "OCAML_MARIADB_PASS" "")
      ~db:(env "OCAML_MARIADB_DB" "mysql") ())

let prepare mariadb query =
  match Mariadb.Nonblocking.prepare mariadb query with
  | `Ok nb -> nonblocking mariadb ~name:"nonblocking prepare" nb
  | `Error e -> failwith "prepare failed"

let execute mariadb stmt params =
  match Mariadb.Nonblocking.Stmt.execute stmt params with
  | `Ok nb -> nonblocking mariadb ~name:"nonblocking execute" nb
  | `Error e -> failwith "execute failed"

let store_result mariadb stmt =
  nonblocking mariadb ~name:"store result"
    (Mariadb.Nonblocking.Stmt.store_result stmt)

let fetch mariadb res =
  nonblocking mariadb ~name:"fetch" (Mariadb.Nonblocking.Res.fetch res)

let close_stmt mariadb stmt =
  nonblocking mariadb ~name:"close result" (Mariadb.Nonblocking.Stmt.close stmt)

let rec with_rows ?(i = 0) mariadb res f =
  match fetch mariadb res with
  | Some row ->
      printf "> %d\n%!" i;
      f row; with_rows ~i:(i + 1) mariadb res f
  | None -> ()

let print_row row =
  Array.iter
    (function
    | `Int i -> printf "%d\n%!" i
    | `Float x -> printf "%f\n%!" x
    | `String s -> printf "%s\n%!" s
    | `Bytes b -> printf "%s\n%!" (Bytes.to_string b)
    | `Null -> printf "NULL\n%!")
    row

let () =
  let mariadb =
    match Mariadb.Nonblocking.init () with
    | Some m -> connect m
    | None -> failwith "cannot init" in
  let query =
    env "OCAML_MARIADB_QUERY" "SELECT * FROM user WHERE LENGTH(user) > ?" in
  let stmt = prepare mariadb query in
  let stmt = execute mariadb stmt [| `String "Problema%" |] in
  let res = store_result mariadb stmt in
  print_endline @@ "#rows: " ^ string_of_int @@ Mariadb.Res.num_rows res;
  with_rows mariadb res print_row;
  close_stmt mariadb stmt;
  printf "done\n%!"
