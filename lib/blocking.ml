module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

type state = [`Unconnected | `Connected | `Tx]
type 's t = ([`Blocking], 's) Common.t
type 's mariadb = 's t

type error = Common.error
type 'a result = ('a, error) Pervasives.result

type flag
type server_option = Common.server_option =
  | Multi_statements of bool

let close =
  B.mysql_close

let connect ?host ?user ?pass ?db ?(port=0) ?socket ?(flags=[]) () =
  (* TODO flags *)
  let connect m =
    match B.mysql_real_connect m host user pass db port socket 0 with
    | Some m -> Ok m
    | None -> Error (2008, "out of memory") in
  match B.mysql_init () with
  | Some m -> connect m
  | None -> Error (2008, "out of memory")

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

  type time = Common.Res.time =
    { year : int
    ; month : int
    ; day : int
    ; hour : int
    ; minute : int
    ; second : int
    }

  type value =
    [ `Int of int
    | `Float of float
    | `String of string
    | `Bytes of bytes
    | `Time of time
    | `Null
    ]

  let fetch res =
    let stmt = res.Common.Res.stmt in
    Gc.compact();
    match B.mysql_stmt_fetch stmt with
    | 0 -> Ok (Some (Common.Res.build_row res))
    | r when r = T.Return_code.no_data -> Ok None
    | r when r = T.Return_code.data_truncated -> Error (2032, "truncated data")
    | _ -> Error (B.mysql_stmt_errno stmt, B.mysql_stmt_error stmt)

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

  let execute' stmt params =
    match execute stmt params with
    | Ok res -> Ok (stmt, res)
    | Error _ as e -> e

  let close stmt =
    let raw = stmt.Common.Stmt.raw in
    if B.mysql_stmt_free_result raw && B.mysql_stmt_close raw then
      Ok ()
    else
      Error (Common.Stmt.error stmt)
end

