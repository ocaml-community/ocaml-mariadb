open Util

module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

let () =
  match B.mysql_library_init 0 None None with
  | 0 -> at_exit B.mysql_library_end
  | _ -> failwith "cannot initialize MariaDB library"

module Field = Common.Field
module Row = Common.Row

type state = [`Unconnected | `Connected | `Tx]
type 's t = ([`Blocking], 's) Common.t
type 's mariadb = 's t

type error = Common.error
type 'a result = ('a, error) Pervasives.result

type flag = Common.flag =
  | Can_handle_expired_passwords
  | Compress
  | Found_rows
  | Ignore_sigpipe
  | Ignore_space
  | Interactive
  | Local_files
  | Multi_results
  | Multi_statements
  | No_schema
  | ODBC
  | SSL
  | Remember_options

type server_option = Common.server_option =
  | Multi_statements of bool

let close =
  B.mysql_close

let connect ?host ?user ?pass ?db ?(port=0) ?socket ?(flags=[]) () =
  let flags = Common.int_of_flags flags in
  let connect m =
    match B.mysql_real_connect m host user pass db port socket flags with
    | Some m -> Ok m
    | None -> Error (2008, "out of memory") in
  match B.mysql_init () with
  | Some m -> connect m
  | None -> Error (2008, "out of memory")

let wrap_unit mariadb f =
  if f mariadb then Ok ()
  else Error (Common.error mariadb)

let set_character_set mariadb charset =
  wrap_unit mariadb ((flip B.mysql_set_character_set) charset)

let select_db mariadb db =
  wrap_unit mariadb ((flip B.mysql_select_db) db)

let change_user mariadb user pass db =
  wrap_unit mariadb (fun m -> B.mysql_change_user m user pass db)

let dump_debug_info mariadb =
  wrap_unit mariadb B.mysql_dump_debug_info

let set_server_option mariadb opt =
  let opt = Common.int_of_server_option opt in
  wrap_unit mariadb ((flip B.mysql_set_server_option) opt)

let ping mariadb =
  wrap_unit mariadb B.mysql_ping

let prepare mariadb query =
  let build_stmt raw =
    if B.mysql_stmt_prepare raw query then
      match Common.Stmt.init mariadb raw with
      | Some stmt -> Ok stmt
      | None -> Error (Common.error mariadb)
    else
      Error (Common.error mariadb) in
  match Common.stmt_init mariadb with
  | Some raw -> build_stmt raw
  | None -> Error (2008, "out of memory")

module Res = struct
  type t = [`Blocking] Common.Res.t

  let fetch (type t) (module R : Row.S with type t = t) res =
    let stmt = res.Common.Res.stmt in
    match B.mysql_stmt_fetch stmt with
    | 0 -> Ok (Some (Common.Res.build_row (module R) res))
    | r when r = T.Return_code.no_data -> Ok None
    | r when r = T.Return_code.data_truncated -> Error (2032, "truncated data")
    | _ -> Error (B.mysql_stmt_errno stmt, B.mysql_stmt_error stmt)

  let stream (type t) (module R : Row.S with type t = t) res =
    Common.Res.stream (module R) res fetch

  let num_rows =
    Common.Res.num_rows

  let affected_rows =
    Common.Res.affected_rows
end

module Stmt = struct
  type state = [`Prepared | `Executed]
  type 's t = ([`Blocking], 's) Common.Stmt.t

  type param =
    [ `Tiny of int
    | `Short of int
    | `Int of int
    | `Float of float
    | `Double of float
    | `String of string
    | `Blob of bytes
    ]

  let execute stmt params =
    let n = B.mysql_stmt_param_count stmt.Common.Stmt.raw in
    if n <> Array.length params then
      Error (0, "parameter count mismatch")
    else begin
      let exec stmt =
        let raw = stmt.Common.Stmt.raw in
        if B.mysql_stmt_execute raw && B.mysql_stmt_store_result raw then
          match Common.Stmt.bind_result stmt with
          | `Ok res -> Ok res
          | `Error e -> Error e
        else
          Error (Common.Stmt.error stmt) in
      match Common.Stmt.bind_params stmt params with
      | `Ok bound -> exec bound
      | `Error e -> Error e
    end

  let close stmt =
    let raw = stmt.Common.Stmt.raw in
    if B.mysql_stmt_free_result raw && B.mysql_stmt_close raw then
      Ok ()
    else
      Error (Common.Stmt.error stmt)
end

module Tx = struct
  let wrap mariadb f =
    if f mariadb then Ok mariadb
    else Error (Common.error mariadb)

  let commit mariadb =
    wrap mariadb B.mysql_commit

  let rollback mariadb =
    wrap mariadb B.mysql_rollback

  let autocommit mariadb auto =
    wrap mariadb ((flip B.mysql_autocommit) auto)
end
