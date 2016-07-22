module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

type mode = [`Blocking | `Nonblocking]

type 'm t = B.mysql constraint 'm = [< mode]
type nonblocking = [`Nonblocking] t

type 'm res = B.res constraint 'm = [< mode]
type nonblocking_res = [`Nonblocking] res

type row = string array
type flag

let init ?mariadb () =
  B.mysql_init ~conn:mariadb ()

let close =
  B.mysql_close

let use_result =
  B.mysql_use_result

let errno =
  B.mysql_errno

let error =
  B.mysql_error

let num_fields =
  B.mysql_num_rows

let num_rows =
  B.mysql_num_rows

let free_result =
  B.mysql_free_result

module Nonblocking = struct
  module Status = Wait_status

  type t = nonblocking
  type res = nonblocking_res

  type options =
    | Nonblocking

  let options mariadb = function
    | Nonblocking ->
        B.mysql_options mariadb T.Mariadb_options.nonblock Ctypes.null

  let init ?mariadb () =
    match B.mysql_init ~conn:mariadb () with
    | Some m -> options m Nonblocking; Some m
    | None -> None

  let connect_start mariadb ?host ?user ?pass ?db ?(port = 0) ?socket
                    ?(flags = []) () =
    (* TODO flags *)
    match
    B.mysql_real_connect_start mariadb host user pass db port socket 0 with
    | 0, Some _ -> `Ok
    | 0, None -> `Error
    | s, _ -> `Wait (Status.of_int s)

  let connect_cont mariadb status =
    match B.mysql_real_connect_cont mariadb (Status.to_int status) with
    | 0, Some _ -> `Ok
    | 0, None -> `Error
    | s, _ -> `Wait (Status.of_int s)

  let handle_query f =
    match f () with
    | 0, 0 -> `Ok
    | 0, _ -> `Error
    | s, _ -> `Wait (Status.of_int s)

  let query_start mariadb query =
    handle_query (fun () -> B.mysql_real_query_start mariadb query)

  let query_cont mariadb status =
    handle_query (fun () -> B.mysql_real_query_cont mariadb status)

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

  let free_result_start res =
    handle_ok_wait (fun () -> B.mysql_free_result_start res)

  let free_result_cont res status =
    handle_ok_wait (fun () -> B.mysql_free_result_cont res status)

  let close_start mariadb =
    handle_ok_wait (fun () -> B.mysql_close_start mariadb)

  let close_cont mariadb status =
    handle_ok_wait (fun () -> B.mysql_close_cont mariadb status)

  let fd =
    B.mysql_get_socket

  let timeout =
    B.mysql_get_timeout_value
end


(*

int mysql_set_character_set_start(int *ret, MYSQL *mysql, const char *csname)
int mysql_set_character_set_cont(int *ret, MYSQL *mysql, int ready_status)
mysql_select_db_start(int *ret, MYSQL *mysql, const char *db)
int mysql_select_db_cont(int *ret, MYSQL *mysql, int ready_status)
int mysql_send_query_start(int *ret, MYSQL *mysql, const char *q, unsigned long
length)
int mysql_send_query_cont(int *ret, MYSQL *mysql, int ready_status)
int mysql_store_result_start(MYSQL_RES **ret, MYSQL *mysql)
int mysql_store_result_cont(MYSQL_RES **ret, MYSQL *mysql, int ready_status)
int mysql_free_result_start(MYSQL_RES *result)
int mysql_free_result_cont(MYSQL_RES *result, int ready_status)
int mysql_close_start(MYSQL *sock)
int mysql_close_cont(MYSQL *sock, int ready_status)
int mysql_change_user_start(my_bool *ret, MYSQL *mysql, const char *user, const
                            char *passwd, const char *db)
int mysql_change_user_cont(my_bool *ret, MYSQL *mysql, int ready_status)

int mysql_query_start(int *ret, MYSQL *mysql, const char *q)
int mysql_query_cont(int *ret, MYSQL *mysql, int ready_status)

int mysql_shutdown_start(int *ret, MYSQL *mysql, enum mysql_enum_shutdown_level
                        shutdown_level)
int mysql_shutdown_cont(int *ret, MYSQL *mysql, int ready_status)
int mysql_dump_debug_info_start(int *ret, MYSQL *mysql)
int mysql_dump_debug_info_cont(int *ret, MYSQL *mysql, int ready_status)
int mysql_refresh_start(int *ret, MYSQL *mysql, unsigned int refresh_options)
int mysql_refresh_cont(int *ret, MYSQL *mysql, int ready_status)
int mysql_kill_start(int *ret, MYSQL *mysql, unsigned long pid)
int mysql_kill_cont(int *ret, MYSQL *mysql, int ready_status)
int mysql_set_server_option_start(int *ret, MYSQL *mysql,
                              enum enum_mysql_set_option option)
int mysql_set_server_option_cont(int *ret, MYSQL *mysql, int ready_status)
int mysql_ping_start(int *ret, MYSQL *mysql)
int mysql_ping_cont(int *ret, MYSQL *mysql, int ready_status)
int mysql_stat_start(const char **ret, MYSQL *mysql)
int mysql_stat_cont(const char **ret, MYSQL *mysql, int ready_status)
int mysql_list_dbs_start(MYSQL_RES **ret, MYSQL *mysql, const char *wild)
int mysql_list_dbs_cont(MYSQL_RES **ret, MYSQL *mysql, int ready_status)
int mysql_list_tables_start(MYSQL_RES **ret, MYSQL *mysql, const char *wild)
int mysql_list_tables_cont(MYSQL_RES **ret, MYSQL *mysql, int ready_status)
int mysql_list_processes_start(MYSQL_RES **ret, MYSQL *mysql)
int mysql_list_processes_cont(MYSQL_RES **ret, MYSQL *mysql, int ready_status)

int mysql_list_fields_start(MYSQL_RES **ret, MYSQL *mysql, const char *table,
                        const char *wild)
int mysql_list_fields_cont(MYSQL_RES **ret, MYSQL *mysql, int ready_status)

int mysql_read_query_result_start(my_bool *ret, MYSQL *mysql)
int mysql_read_query_result_cont(my_bool *ret, MYSQL *mysql, int ready_status)

int mysql_stmt_prepare_start(int *ret, MYSQL_STMT *stmt, const char *query,
                         unsigned long length)
int mysql_stmt_prepare_cont(int *ret, MYSQL_STMT *stmt, int ready_status)

int mysql_stmt_execute_start(int *ret, MYSQL_STMT *stmt)
int mysql_stmt_execute_cont(int *ret, MYSQL_STMT *stmt, int ready_status)

int mysql_stmt_fetch_start(int *ret, MYSQL_STMT *stmt)
int mysql_stmt_fetch_cont(int *ret, MYSQL_STMT *stmt, int ready_status)

int mysql_stmt_store_result_start(int *ret, MYSQL_STMT *stmt)
int mysql_stmt_store_result_cont(int *ret, MYSQL_STMT *stmt, int ready_status)

int mysql_stmt_close_start(my_bool *ret, MYSQL_STMT *stmt)
int mysql_stmt_close_cont(my_bool *ret, MYSQL_STMT *stmt, int ready_status)

int mysql_stmt_reset_start(my_bool *ret, MYSQL_STMT *stmt)
int mysql_stmt_reset_cont(my_bool *ret, MYSQL_STMT *stmt, int ready_status)

int mysql_stmt_free_result_start(my_bool *ret, MYSQL_STMT *stmt)
int mysql_stmt_free_result_cont(my_bool *ret, MYSQL_STMT *stmt, int
ready_status)

int mysql_stmt_send_long_data_start(my_bool *ret, MYSQL_STMT *stmt,
                                unsigned int param_number,
                                                                const char
                                                                *data, unsigned
                                                                long length)
int mysql_stmt_send_long_data_cont(my_bool *ret, MYSQL_STMT *stmt, int
ready_status)

int mysql_commit_start(my_bool *ret, MYSQL *mysql)
int mysql_commit_cont(my_bool *ret, MYSQL *mysql, int ready_status)

int mysql_rollback_start(my_bool *ret, MYSQL *mysql)
int mysql_rollback_cont(my_bool *ret, MYSQL *mysql, int ready_status)


int mysql_autocommit_start(my_bool *ret, MYSQL *mysql, my_bool auto_mode)
int mysql_autocommit_cont(my_bool *ret, MYSQL *mysql, int ready_status)


int mysql_next_result_start(int *ret, MYSQL *mysql)
int mysql_next_result_cont(int *ret, MYSQL *mysql, int ready_status)

int mysql_stmt_next_result_start(int *ret, MYSQL_STMT *stmt)
int mysql_stmt_next_result_cont(int *ret, MYSQL_STMT *stmt, int ready_status)

*)
