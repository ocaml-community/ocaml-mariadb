module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

type mode = [`Blocking | `Nonblocking]

type 'm t = B.Types.mysql constraint 'm = [< mode]
type 'm mariadb = 'm t

type row = string array

type flag

type server_option =
  | Multi_statements of bool

module Error = struct
  type t = int * string

  let create mysql =
    (B.mysql_errno mysql, B.mysql_error mysql)

  let errno = fst
  let message = snd
end

module Res = struct
  type 'm t = B.Types.res constraint 'm = [< mode]

  let num_fields =
    B.mysql_num_rows

  let num_rows =
    B.mysql_num_rows

  let free =
    B.mysql_free_result
end

module Stmt = struct
  type 'm t = B.Types.stmt constraint 'm = [< mode]

  let init =
    B.mysql_stmt_init

  module Error = struct
    type 'm stmt = 'm t
    type t = int * string

    let create stmt =
      (B.mysql_stmt_errno stmt, B.mysql_error stmt)

    let errno = fst
    let message = snd
  end
end

module Nonblocking = struct
  module Status = Wait_status

  type t = [`Nonblocking] mariadb
  type res = [`Nonblocking] Res.t
  type stmt = [`Nonblocking] Stmt.t

  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Error.t]

  type options =
    | Nonblocking

  let options mariadb = function
    | Nonblocking ->
        B.mysql_options mariadb T.Mariadb_options.nonblock Ctypes.null

  let init ?mariadb () =
    match B.mysql_init ~mysql:mariadb () with
    | Some m -> options m Nonblocking; Some m
    | None -> None

  let handle_opt mariadb f =
    match f mariadb with
    | 0, Some r -> `Ok r
    | 0, None -> `Error (Error.create mariadb)
    | s, _ -> `Wait (Status.of_int s)

  let handle_unit mariadb f =
    match handle_opt mariadb f with
    | `Ok _ -> `Ok ()
    | `Wait s -> `Wait s
    | `Error e -> `Error e

  let handle_int mariadb f =
    match f mariadb with
    | 0, 0 -> `Ok ()
    | 0, _ -> `Error (Error.create mariadb)
    | s, _ -> `Wait (Status.of_int s)

  let handle_char mariadb f =
    match f mariadb with
    | 0, '\000' -> `Ok ()
    | 0, _ -> `Error (Error.create mariadb)
    | s, _ -> `Wait (Status.of_int s)

  let connect_start mariadb ?host ?user ?pass ?db ?(port = 0) ?socket
                    ?(flags = []) () =
    (* TODO flags *)
    handle_unit mariadb
      (fun m -> B.mysql_real_connect_start m host user pass db port socket 0)

  let connect_cont mariadb status =
    handle_unit mariadb
      (fun m -> B.mysql_real_connect_cont m (Status.to_int status))

  let query_start mariadb query =
    handle_int mariadb (fun m -> B.mysql_real_query_start m query)

  let query_cont mariadb status =
    handle_int mariadb (fun m -> B.mysql_real_query_cont m status)

  let handle_fetch_row f =
    match f () with
    | 0, Some row -> `Ok row
    | 0, None -> `Done
    | s, _ -> `Wait (Status.of_int s)

  let fetch_row_start res =
    handle_fetch_row (fun () -> B.mysql_fetch_row_start res)

  let fetch_row_cont res status =
    handle_fetch_row (fun () -> B.mysql_fetch_row_cont res status)

  let handle_ok_wait f =
    match f () with
    | 0 -> `Ok
    | s -> `Wait (Status.of_int s)

  let close_start mariadb =
    handle_ok_wait (fun () -> B.mysql_close_start mariadb)

  let close_cont mariadb status =
    handle_ok_wait (fun () -> B.mysql_close_cont mariadb status)

  let fd =
    B.mysql_get_socket

  let timeout =
    B.mysql_get_timeout_value

  let set_charset_start mariadb charset =
    handle_unit mariadb (fun m -> B.mysql_set_character_set_start m charset)

  let set_charset_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_set_character_set_cont m status)

  let select_db_start mariadb db =
    handle_unit mariadb (fun m -> B.mysql_select_db_start m db)

  let select_db_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_select_db_cont m status)

  let change_user_start mariadb user pass db =
    handle_unit mariadb (fun m -> B.mysql_change_user_start m user pass db)

  let change_user_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_change_user_cont m status)

  let dump_debug_info_start mariadb =
    handle_unit mariadb (fun m -> B.mysql_dump_debug_info_start m)

  let dump_debug_info_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_dump_debug_info_cont m status)

  let set_server_option_start mariadb opt =
    let opt =
      match opt with
      | Multi_statements true -> T.Mariadb_server_options.multi_statements_on
      | Multi_statements false -> T.Mariadb_server_options.multi_statements_off
    in
    handle_unit mariadb (fun m -> B.mysql_set_server_option_start m opt)

  let set_server_option_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_set_server_option_cont m status)

  let ping_start mariadb =
    handle_unit mariadb (fun m -> B.mysql_ping_start m)

  let ping_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_ping_cont m status)

  let list_dbs_start mariadb wild =
    handle_opt mariadb (fun m -> B.mysql_list_dbs_start m wild)

  let list_dbs_cont mariadb status =
    handle_opt mariadb (fun m -> B.mysql_list_dbs_cont m status)

  let list_tables_start mariadb wild =
    handle_opt mariadb (fun m -> B.mysql_list_tables_start m wild)

  let list_tables_cont mariadb status =
    handle_opt mariadb (fun m -> B.mysql_list_tables_cont m status)

  let handle_next_result obj f errf =
    match f obj with
    | 0, 0 -> `Ok true
    | 0, -1 -> `Ok false
    | 0, _ -> `Error (errf obj)
    | s, _ -> `Wait (Status.of_int s)

  let next_result_start mariadb =
    handle_next_result
      mariadb (fun m -> B.mysql_next_result_start m) Error.create

  let next_result_cont mariadb status =
    handle_next_result
      mariadb (fun m -> B.mysql_next_result_cont m status) Error.create

  module Res = struct
    let free_start res =
      handle_ok_wait (fun () -> B.mysql_free_result_start res)

    let free_cont res status =
      handle_ok_wait (fun () -> B.mysql_free_result_cont res status)
  end

  module Stmt = struct
    let prepare_start stmt query =
      handle_int stmt (fun s -> B.mysql_stmt_prepare_start s query)

    let prepare_cont stmt status =
      handle_int stmt (fun s -> B.mysql_stmt_prepare_cont s status)

    let execute_start stmt query =
      handle_int stmt (fun s -> B.mysql_stmt_execute_start s)

    let execute_cont stmt status =
      handle_int stmt (fun s -> B.mysql_stmt_execute_cont s status)

    let fetch_start stmt query =
      handle_int stmt (fun s -> B.mysql_stmt_fetch_start s)

    let fetch_cont stmt status =
      handle_int stmt (fun s -> B.mysql_stmt_fetch_cont s status)

    let store_result_start stmt query =
      handle_int stmt (fun s -> B.mysql_stmt_store_result_start s)

    let store_result_cont stmt status =
      handle_int stmt (fun s -> B.mysql_stmt_store_result_cont s status)

    let close_start stmt query =
      handle_char stmt (fun s -> B.mysql_stmt_close_start s)

    let close_cont stmt status =
      handle_char stmt (fun s -> B.mysql_stmt_close_cont s status)

    let reset_start stmt query =
      handle_char stmt (fun s -> B.mysql_stmt_reset_start s)

    let reset_cont stmt status =
      handle_char stmt (fun s -> B.mysql_stmt_reset_cont s status)

    let free_result_start stmt =
      handle_char stmt (fun s -> B.mysql_stmt_free_result_start s)

    let free_result_cont stmt status =
      handle_char stmt (fun s -> B.mysql_stmt_free_result_cont s status)

    let next_result_start stmt =
      handle_next_result
        stmt (fun s -> B.mysql_stmt_next_result_start s) Stmt.Error.create

    let next_result_cont stmt status =
      handle_next_result
        stmt (fun s -> B.mysql_stmt_next_result_cont s status) Stmt.Error.create
  end

  module Tx = struct
    let commit_start mariadb =
      handle_char mariadb (fun m -> B.mysql_commit_start m)

    let commit_cont mariadb status =
      handle_char mariadb (fun m -> B.mysql_commit_cont m status)

    let rollback_start mariadb =
      handle_char mariadb (fun m -> B.mysql_rollback_start m)

    let rollback_cont mariadb status =
      handle_char mariadb (fun m -> B.mysql_rollback_cont m status)

    let autocommit_start mariadb auto =
      handle_char mariadb (fun m -> B.mysql_autocommit_start m auto)

    let autocommit_cont mariadb status =
      handle_char mariadb (fun m -> B.mysql_autocommit_cont m status)
  end
end

let init ?mariadb () =
  B.mysql_init ~mysql:mariadb ()

let close =
  B.mysql_close

let use_result =
  B.mysql_use_result
