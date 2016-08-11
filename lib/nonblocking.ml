open Util

module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

module Field = Common.Field
module Row = Common.Row
module Status = Wait_status

type t = [`Nonblocking] Common.t
type mariadb = t

type error = Common.error
type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

type server_option = Common.server_option

type 'a start = unit -> 'a result
type 'a cont = Status.t -> 'a result
type 'a nonblocking = 'a start * 'a cont

let map_result f = function
  | `Ok x -> `Ok (f x)
  | `Wait _ as w -> w
  | `Error _ as e -> e

type options =
  | Nonblocking

let options mariadb = function
  | Nonblocking ->
      B.mysql_options mariadb T.Options.nonblock Ctypes.null

let init () =
  match B.mysql_init () with
  | Some raw ->
      options raw Nonblocking;
      Some raw
  | None ->
      None

let handle_opt mariadb f =
  match f mariadb with
  | 0, Some r -> `Ok r
  | 0, None -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_int_ret mariadb f =
  match f mariadb with
  | 0, 0 -> `Ok mariadb
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_int mariadb f =
  match f mariadb with
  | 0, 0 -> `Ok ()
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_char mariadb f =
  match f mariadb with
  | 0, '\000' -> `Ok ()
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let connect_start mariadb host user pass db port socket flags () =
  handle_opt mariadb
    (fun m ->
      B.mysql_real_connect_start m host user pass db port socket flags)

let connect_cont mariadb status =
  handle_opt mariadb
    (fun m -> B.mysql_real_connect_cont m (Status.to_int status))

let connect mariadb ?host ?user ?pass ?db ?(port=0) ?socket ?(flags=[]) () =
  let flags = Common.int_of_flags flags in
  let start = connect_start mariadb host user pass db port socket flags in
  let cont = connect_cont mariadb in
  (start, cont)

let handle_ok_wait mariadb f =
  match f mariadb with
  | 0 -> `Ok
  | s -> `Wait (Status.of_int s)

let close_start mariadb () =
  handle_ok_wait mariadb (fun raw -> B.mysql_close_start raw)

let close_cont mariadb status =
  handle_ok_wait mariadb (fun raw -> B.mysql_close_cont raw status)

let close mariadb =
  (close_start mariadb, close_cont mariadb)

let fd =
  B.mysql_get_socket

let timeout =
  B.mysql_get_timeout_value

let set_character_set_start mariadb charset () =
  handle_int mariadb ((flip B.mysql_set_character_set_start) charset)

let set_character_set_cont mariadb status =
  handle_int mariadb ((flip B.mysql_set_character_set_cont) status)

let set_character_set mariadb charset =
  (set_character_set_start mariadb charset, set_character_set_cont mariadb)

let select_db_start mariadb db () =
  handle_int mariadb ((flip B.mysql_select_db_start) db)

let select_db_cont mariadb status =
  handle_int mariadb ((flip B.mysql_select_db_cont) status)

let select_db mariadb db =
  (select_db_start mariadb db, select_db_cont mariadb)

let change_user_start mariadb user pass db () =
  handle_char mariadb (fun m -> B.mysql_change_user_start m user pass db)

let change_user_cont mariadb status =
  handle_char mariadb ((flip B.mysql_change_user_cont) status)

let change_user mariadb user pass db =
  (change_user_start mariadb user pass db, change_user_cont mariadb)

let dump_debug_info_start mariadb () =
  handle_int mariadb B.mysql_dump_debug_info_start

let dump_debug_info_cont mariadb status =
  handle_int mariadb ((flip B.mysql_dump_debug_info_cont) status)

let dump_debug_info mariadb =
  (dump_debug_info_start mariadb, dump_debug_info_cont mariadb)

let set_server_option_start mariadb opt () =
  let opt = Common.int_of_server_option opt in
  handle_int mariadb ((flip B.mysql_set_server_option_start) opt)

let set_server_option_cont mariadb status =
  handle_int mariadb ((flip B.mysql_set_server_option_cont) status)

let set_server_option mariadb opt =
  (set_server_option_start mariadb opt, set_server_option_cont mariadb)

let ping_start mariadb () =
  handle_int mariadb B.mysql_ping_start

let ping_cont mariadb status =
  handle_int mariadb ((flip B.mysql_ping_cont) status)

let ping mariadb =
  (ping_start mariadb, ping_cont mariadb)

let autocommit_start mariadb auto () =
  handle_char mariadb ((flip B.mysql_autocommit_start) auto)

let autocommit_cont mariadb status =
  handle_char mariadb ((flip B.mysql_autocommit_cont) status)

let autocommit mariadb auto =
  (autocommit_start mariadb auto, autocommit_cont mariadb)

let build_stmt mariadb raw =
  match Common.Stmt.init mariadb raw with
  | Some stmt -> `Ok stmt
  | None -> `Error (Common.error mariadb)

let handle_prepare mariadb raw f =
  match f raw with
  | 0, 0 -> build_stmt mariadb raw
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let prepare_start mariadb raw_stmt query () =
  handle_prepare mariadb raw_stmt ((flip B.mysql_stmt_prepare_start) query)

let prepare_cont mariadb raw_stmt status =
  handle_prepare mariadb raw_stmt ((flip B.mysql_stmt_prepare_cont) status)

let prepare mariadb query =
  match Common.stmt_init mariadb with
  | Some raw -> `Ok (prepare_start mariadb raw query, prepare_cont mariadb raw)
  | None -> `Error (Common.error mariadb)

module Res = struct
  type t = [`Nonblocking] Common.Res.t

  let num_rows =
    Common.Res.num_rows

  let affected_rows =
    Common.Res.affected_rows

  let handle_fetch (type t) (module R : Row.S with type t = t) res f =
    let stmt = res.Common.Res.stmt in
    match f stmt with
    | 0, 0 -> `Ok (Some (Common.Res.build_row (module R) res))
    | 0, 1 -> `Error (B.mysql_stmt_errno stmt, B.mysql_stmt_error stmt)
    | 0, r when r = T.Return_code.no_data -> `Ok None
    | 0, r when r = T.Return_code.data_truncated ->
        `Error (2032, "truncated data")
    | s, _ -> `Wait (Status.of_int s)

  let fetch_start (type t) (module R : Row.S with type t = t) res () =
    handle_fetch (module R) res B.mysql_stmt_fetch_start

  let fetch_cont (type t) (module R : Row.S with type t = t) res status =
    handle_fetch (module R) res ((flip B.mysql_stmt_fetch_cont) status)

  let fetch (type t) (module R : Row.S with type t = t) res =
    (fetch_start (module R) res, fetch_cont (module R) res)

  let handle_free res f =
    let stmt = res.Common.Res.stmt in
    match f stmt with
    | 0, '\000' -> `Ok ()
    | 0, _ -> `Error (B.mysql_stmt_errno stmt, B.mysql_stmt_error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let free_start res () =
    handle_free res B.mysql_stmt_free_result_start

  let free_cont res status =
    handle_free res ((flip B.mysql_stmt_free_result_cont) status)

  let free res =
    (free_start res, free_cont res)
end

module Stmt = struct
  type t = [`Nonblocking] Common.Stmt.t
  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

  let init =
    Common.Stmt.init

  let handle_execute stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, 0 -> `Ok stmt
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let execute_start stmt () =
    handle_execute stmt B.mysql_stmt_execute_start

  let execute_cont stmt status =
    handle_execute stmt ((flip B.mysql_stmt_execute_cont) status)

  let execute stmt params =
    let n = B.mysql_stmt_param_count stmt.Common.Stmt.raw in
    if n <> Array.length params then
      `Error (0, "parameter count mismatch")
    else begin
      match Common.Stmt.bind_params stmt params with
      | `Ok bound -> `Ok (execute_start bound, execute_cont bound)
      | `Error _ as err -> err
    end

  let handle_store_result stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, 0 -> Common.Stmt.bind_result stmt
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let store_result_start stmt () =
    handle_store_result stmt B.mysql_stmt_store_result_start

  let store_result_cont stmt status =
    handle_store_result stmt ((flip B.mysql_stmt_store_result_cont) status)

  let store_result stmt =
    (store_result_start stmt, store_result_cont stmt)

  let handle_char_unit stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, '\000' -> `Ok ()
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let close_start stmt () =
    handle_char_unit stmt (fun s -> B.mysql_stmt_close_start s)

  let close_cont stmt status =
    handle_char_unit stmt (fun s -> B.mysql_stmt_close_cont s status)

  let close stmt =
    (close_start stmt, close_cont stmt)

  let handle_next stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, 0 -> `Ok true
    | 0, -1 -> `Ok false
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let next_result_start stmt =
    handle_next stmt (B.mysql_stmt_next_result_start)

  let next_result_cont stmt status =
    handle_next stmt ((flip B.mysql_stmt_next_result_cont) status)
end

module type Wait = sig
  val wait : t -> Status.t -> Status.t
end

module Make (W : Wait) = struct
  type t = mariadb

  type error = int * string
  type 'a result = ('a, error) Pervasives.result

  type flag = Common.flag =
    | Client_can_handle_expired_passwords
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

  type protocol = Common.protocol =
    | Default
    | Tcp
    | Socket
    | Pipe
    | Memory

  type client_option = Common.client_option =
    | Connect_timeout of int
    | Compress
    | Named_pipe of string
    | Init_command of string
    | Read_default_file of string
    | Read_default_group of string
    | Set_charset_dir of string
    | Set_charset_name of string
    | Local_infile of bool
    | Protocol of protocol
    | Shared_memory_base_name of string
    | Read_timeout of int
    | Write_timeout of int
    | Secure_auth of bool
    | Report_data_truncation of bool
    | Reconnect of bool
    | Ssl_verify_server_cert of bool
    | Plugin_dir of string
    | Default_auth of string
    | Bind of string
    | Ssl_key of string
    | Ssl_cert of string
    | Ssl_ca of string
    | Ssl_capath of string
    | Ssl_cipher of string
    | Ssl_crl of string
    | Ssl_crlpath of string
    | Connect_attr_reset
    | Connect_attr_add of string * string
    | Connect_attr_delete of string
    | Server_public_key of string
    | Enable_cleartext_plugin of bool
    | Can_handle_expired_passwords of bool
    | Use_thread_specific_memory of bool

  type server_option = Common.server_option =
    | Multi_statements of bool

  let rec nonblocking m (f, g) =
    match f () with
    | `Ok v -> Ok v
    | `Wait s -> let s = W.wait m s in nonblocking m ((fun () -> g s), g)
    | `Error e -> Error e

  let rec nonblocking_noerr m (f, g) =
    match f () with
    | `Ok -> ()
    | `Wait s -> let s = W.wait m s in nonblocking_noerr m ((fun () -> g s), g)

  module Field = Field
  module Row = Row

  module Res = struct
    type t = Res.t

    let fetch (type t) (module R : Row.S with type t = t) res =
      nonblocking res.Common.Res.mariadb (Res.fetch (module R) res)

    let stream (type t) (module R : Row.S with type t = t) res =
      Common.Res.stream (module R) res fetch

    let num_rows =
      Res.num_rows

    let affected_rows =
      Res.affected_rows

    let free res =
      nonblocking res.Common.Res.mariadb (Res.free res)
  end

  module Stmt = struct
    type t = Stmt.t

    type param =
      [ `Tiny of int
      | `Short of int
      | `Int of int
      | `Float of float
      | `Double of float
      | `String of string
      | `Blob of bytes
      ]

    let handle_execute = function
      | Ok stmt -> nonblocking stmt.Common.Stmt.mariadb (Stmt.store_result stmt)
      | Error _ as e -> e

    let execute stmt ps =
      match Stmt.execute stmt ps with
      | `Ok nb -> nonblocking stmt.Common.Stmt.mariadb nb |> handle_execute
      | `Error e -> Error e

    let free_res stmt =
      let handle_free f =
        match f stmt.Common.Stmt.raw with
        | 0, '\000' -> `Ok ()
        | 0, _ -> `Error (Common.Stmt.error stmt)
        | s, _ -> `Wait (Status.of_int s) in
      let start () = handle_free B.mysql_stmt_free_result_start in
      let cont s = handle_free ((flip B.mysql_stmt_free_result_cont) s) in
      nonblocking stmt.Common.Stmt.mariadb (start, cont)

    let close stmt =
      match free_res stmt with
      | Ok () -> nonblocking stmt.Common.Stmt.mariadb (Stmt.close stmt)
      | Error _ as e -> e
  end

  let connect ?host ?user ?pass ?db ?(port=0) ?socket ?(flags=[]) () =
    match init () with
    | Some m ->
        nonblocking m (connect m ?host ?user ?pass ?db ~port ?socket ~flags ())
    | None ->
        Error (2008, "out of memory")

  let close m = nonblocking_noerr m (close m)

  let set_character_set m c = nonblocking m (set_character_set m c)

  let select_db m db = nonblocking m (select_db m db)

  let change_user m user pass db = nonblocking m (change_user m user pass db)

  let dump_debug_info m = nonblocking m (dump_debug_info m)

  let set_client_option = Common.set_client_option

  let set_server_option m opt = nonblocking m (set_server_option m opt)

  let ping m = nonblocking m (ping m)

  let autocommit m b = nonblocking m (autocommit m b)

  let prepare m q =
    match prepare m q with
    | `Ok nb -> nonblocking m nb
    | `Error e -> Error e
end
