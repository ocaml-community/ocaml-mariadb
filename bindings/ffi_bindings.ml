open Ctypes

module Types (F: Cstubs.Types.TYPE) = struct
  open F

  module Protocol = struct
    let default = constant "MYSQL_PROTOCOL_DEFAULT" int
    let tcp = constant "MYSQL_PROTOCOL_TCP" int
    let socket = constant "MYSQL_PROTOCOL_SOCKET" int
    let pipe = constant "MYSQL_PROTOCOL_PIPE" int
    let memory = constant "MYSQL_PROTOCOL_MEMORY" int
  end

  module Options = struct
    let connect_timeout = constant "MYSQL_OPT_CONNECT_TIMEOUT" int
    let compress = constant "MYSQL_OPT_COMPRESS" int
    let named_pipe = constant "MYSQL_OPT_NAMED_PIPE" int
    let init_command = constant "MYSQL_INIT_COMMAND" int
    let read_default_file = constant "MYSQL_READ_DEFAULT_FILE" int
    let read_default_group = constant "MYSQL_READ_DEFAULT_GROUP" int
    let set_charset_dir = constant "MYSQL_SET_CHARSET_DIR" int
    let set_charset_name = constant "MYSQL_SET_CHARSET_NAME" int
    let local_infile = constant "MYSQL_OPT_LOCAL_INFILE" int
    let protocol = constant "MYSQL_OPT_PROTOCOL" int
    let shared_memory_base_name = constant "MYSQL_SHARED_MEMORY_BASE_NAME" int
    let read_timeout = constant "MYSQL_OPT_READ_TIMEOUT" int
    let write_timeout = constant "MYSQL_OPT_WRITE_TIMEOUT" int
    let secure_auth = constant "MYSQL_SECURE_AUTH" int
    let report_data_truncation = constant "MYSQL_REPORT_DATA_TRUNCATION" int
    let reconnect = constant "MYSQL_OPT_RECONNECT" int
    let ssl_verify_server_cert = constant "MYSQL_OPT_SSL_VERIFY_SERVER_CERT" int
    let plugin_dir = constant "MYSQL_PLUGIN_DIR" int
    let default_auth = constant "MYSQL_DEFAULT_AUTH" int
    let bind = constant "MYSQL_OPT_BIND" int
    let ssl_key = constant "MYSQL_OPT_SSL_KEY" int
    let ssl_cert = constant "MYSQL_OPT_SSL_CERT" int
    let ssl_ca = constant "MYSQL_OPT_SSL_CA" int
    let ssl_capath = constant "MYSQL_OPT_SSL_CAPATH" int
    let ssl_cipher = constant "MYSQL_OPT_SSL_CIPHER" int
    let ssl_crl = constant "MYSQL_OPT_SSL_CRL" int
    let ssl_crlpath = constant "MYSQL_OPT_SSL_CRLPATH" int
    let connect_attr_reset = constant "MYSQL_OPT_CONNECT_ATTR_RESET" int
    let connect_attr_add = constant "MYSQL_OPT_CONNECT_ATTR_ADD" int
    let connect_attr_delete = constant "MYSQL_OPT_CONNECT_ATTR_DELETE" int
    let server_public_key = constant "MYSQL_SERVER_PUBLIC_KEY" int
    let enable_cleartext_plugin = constant "MYSQL_ENABLE_CLEARTEXT_PLUGIN" int
    let can_handle_expired_passwords =
      constant "MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS" int
    let nonblock = constant "MYSQL_OPT_NONBLOCK" int
    let use_thread_specific_memory =
      constant "MYSQL_OPT_USE_THREAD_SPECIFIC_MEMORY" int
  end

  module Flags = struct
    let can_handle_expired_passwords =
      constant "CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS" int
    let compress = constant "CLIENT_COMPRESS" int
    let found_rows = constant "CLIENT_FOUND_ROWS" int
    let ignore_sigpipe = constant "CLIENT_IGNORE_SIGPIPE" int
    let ignore_space = constant "CLIENT_IGNORE_SPACE" int
    let interactive = constant "CLIENT_INTERACTIVE" int
    let local_files = constant "CLIENT_LOCAL_FILES" int
    let multi_results = constant "CLIENT_MULTI_RESULTS" int
    let multi_statements = constant "CLIENT_MULTI_STATEMENTS" int
    let no_schema = constant "CLIENT_NO_SCHEMA" int
    let odbc = constant "CLIENT_ODBC" int
    let ssl = constant "CLIENT_SSL" int
    let remember_options = constant "CLIENT_REMEMBER_OPTIONS" int
  end

  module Server_options = struct
    let multi_statements_on = constant "MYSQL_OPTION_MULTI_STATEMENTS_ON" int
    let multi_statements_off = constant "MYSQL_OPTION_MULTI_STATEMENTS_OFF" int
  end

  module Wait_status = struct
    let read = constant "MYSQL_WAIT_READ" int
    let write = constant "MYSQL_WAIT_WRITE" int
    let except = constant "MYSQL_WAIT_EXCEPT" int
    let timeout = constant "MYSQL_WAIT_TIMEOUT" int
  end

  module Type = struct
    let null = constant "MYSQL_TYPE_NULL" int
    let tiny = constant "MYSQL_TYPE_TINY" int
    let year = constant "MYSQL_TYPE_YEAR" int
    let short = constant "MYSQL_TYPE_SHORT" int
    let int24 = constant "MYSQL_TYPE_INT24" int
    let long = constant "MYSQL_TYPE_LONG" int
    let float = constant "MYSQL_TYPE_FLOAT" int
    let long_long = constant "MYSQL_TYPE_LONGLONG" int
    let double = constant "MYSQL_TYPE_DOUBLE" int
    let decimal = constant "MYSQL_TYPE_DECIMAL" int
    let new_decimal = constant "MYSQL_TYPE_NEWDECIMAL" int
    let string = constant "MYSQL_TYPE_STRING" int
    let var_string = constant "MYSQL_TYPE_VAR_STRING" int
    let tiny_blob = constant "MYSQL_TYPE_TINY_BLOB" int
    let blob = constant "MYSQL_TYPE_BLOB" int
    let medium_blob = constant "MYSQL_TYPE_MEDIUM_BLOB" int
    let long_blob = constant "MYSQL_TYPE_LONG_BLOB" int
    let bit = constant "MYSQL_TYPE_BIT" int
    let time = constant "MYSQL_TYPE_TIME" int
    let date = constant "MYSQL_TYPE_DATE" int
    let datetime = constant "MYSQL_TYPE_DATETIME" int
    let timestamp = constant "MYSQL_TYPE_TIMESTAMP" int
  end

  module Stmt_attr = struct
    let update_max_length = constant "STMT_ATTR_UPDATE_MAX_LENGTH" int
    let cursor_type = constant "STMT_ATTR_CURSOR_TYPE" int
    let prefetch_rows = constant "STMT_ATTR_PREFETCH_ROWS" int
  end

  module Return_code = struct
    let no_data = constant "MYSQL_NO_DATA" int
    let data_truncated = constant "MYSQL_DATA_TRUNCATED" int
  end

  module Bind = struct
    type bind
    type t = bind structure
    let t : t typ = structure "st_mysql_bind"

    let length = field t "length" (ptr ulong)
    let is_null = field t "is_null" (ptr char)
    let buffer = field t "buffer" (ptr void)
    let error = field t "error" (ptr char)
    let buffer_length = field t "buffer_length" ulong
    let buffer_type = field t "buffer_type" int
    let is_unsigned = field t "is_unsigned" char

    let () = seal t
  end

  module Field = struct
    type field
    type t = field structure
    let t : t typ = structure "st_mysql_field"

    let name = field t "name" string
    let max_length = field t "max_length" ulong
    let flags = field t "flags" uint
    let typ = field t "type" int

    let () = seal t

    module Flags = struct
      let not_null = constant "NOT_NULL_FLAG" uint
      let unsigned = constant "UNSIGNED_FLAG" uint
    end
  end

  module Time = struct
    type time
    type t = time structure
    let t : t typ = structure "st_mysql_time"

    let year = field t "year" uint
    let month = field t "month" uint
    let day = field t "day" uint
    let hour = field t "hour" uint
    let minute = field t "minute" uint
    let second = field t "second" uint

    let () = seal t
  end
end

module Foreign_bindings = struct
  open Foreign

  let foreign name typ = foreign name typ
    ~from:Dl.(dlopen ~filename:"libmysqlclient.so" ~flags:[RTLD_NOW])

  module Types = struct
    type mysql = unit ptr
    let mysql : mysql typ = ptr void

    type mysql_opt = unit ptr option
    let mysql_opt : mysql_opt typ = ptr_opt void

    type res = unit ptr
    let res : res typ = ptr void

    type res_opt = unit ptr option
    let res_opt : res_opt typ = ptr_opt void

    type row = char ptr ptr
    let row : row typ = ptr (ptr char)

    type row_opt = char ptr ptr option
    let row_opt : row_opt typ = ptr_opt (ptr char)

    type stmt = unit ptr
    let stmt : stmt typ = ptr void

    type stmt_opt = unit ptr option
    let stmt_opt : stmt_opt typ = ptr_opt void

    type field = unit ptr
    let field : field typ = ptr void

    type my_bool = char
    let my_bool : char typ = char
  end

  module T = Types

  (* Common API *)

  let mysql_library_init = foreign "mysql_server_init"
    (int @-> ptr_opt (ptr char) @-> ptr_opt (ptr char) @-> returning int)

  let mysql_library_end = foreign "mysql_server_end"
    (void @-> returning void)

  let mysql_init = foreign "mysql_init"
    (T.mysql_opt @-> returning T.mysql_opt)

  let mysql_close = foreign "mysql_close"
    (T.mysql @-> returning void)

  let mysql_options = foreign "mysql_options"
    (T.mysql @-> int @-> ptr void @-> returning int)

  let mysql_options4 = foreign "mysql_options4"
    (T.mysql @-> int @-> ptr void @-> ptr void @-> returning int)

  let mysql_num_fields = foreign "mysql_num_fields"
    (T.res @-> returning int)

  let mysql_errno = foreign "mysql_errno"
    (T.mysql @-> returning int)

  let mysql_error = foreign "mysql_error"
    (T.mysql @-> returning string)

  let mysql_stmt_init = foreign "mysql_stmt_init"
    (T.mysql @-> returning T.stmt_opt)

  let mysql_stmt_errno = foreign "mysql_stmt_errno"
    (T.stmt @-> returning int)

  let mysql_stmt_error = foreign "mysql_stmt_error"
    (T.stmt @-> returning string)

  let mysql_stmt_attr_set = foreign "mysql_stmt_attr_set"
    (T.stmt @-> int @-> ptr void @-> returning T.my_bool)

  (* XXX ptr void because we can't access Bind.t here *)
  let mysql_stmt_bind_param = foreign "mysql_stmt_bind_param"
    (T.stmt @-> ptr void @-> returning T.my_bool)

  let mysql_stmt_param_count = foreign "mysql_stmt_param_count"
    (T.stmt @-> returning ulong)

  let mysql_stmt_execute = foreign "mysql_stmt_execute"
    (T.stmt @-> returning int)

  let mysql_stmt_result_metadata = foreign "mysql_stmt_result_metadata"
    (T.stmt @-> returning T.res_opt)

  (* XXX ptr void because we can't access Field.t here *)
  let mysql_fetch_field_direct = foreign "mysql_fetch_field_direct"
    (T.res @-> uint @-> returning (ptr void))

  (* XXX ptr void because we can't access Bind.t here *)
  let mysql_stmt_bind_result = foreign "mysql_stmt_bind_result"
    (T.stmt @-> ptr void @-> returning T.my_bool)

  let mysql_stmt_num_rows = foreign "mysql_stmt_num_rows"
    (T.stmt @-> returning ullong)

  let mysql_stmt_affected_rows = foreign "mysql_stmt_affected_rows"
    (T.res @-> returning ullong)

  (* Blocking API *)

  let mysql_real_connect = foreign "mysql_real_connect"
    (T.mysql @-> ptr_opt char @-> ptr_opt char @->
     ptr_opt char @-> ptr_opt char @-> uint @-> ptr_opt char @-> ulong @->
     returning T.mysql_opt)

  let mysql_commit = foreign "mysql_commit"
    (T.mysql @-> returning T.my_bool)

  let mysql_rollback = foreign "mysql_rollback"
    (T.mysql @-> returning T.my_bool)

  let mysql_autocommit = foreign "mysql_autocommit"
    (T.mysql @-> T.my_bool @-> returning T.my_bool)

  let mysql_set_character_set = foreign "mysql_set_character_set"
    (T.mysql @-> ptr char @-> returning int)

  let mysql_select_db = foreign "mysql_select_db"
    (T.mysql @-> ptr char @-> returning int)

  let mysql_change_user = foreign "mysql_change_user"
    (T.mysql @-> ptr char @-> ptr char @-> ptr_opt char @-> returning T.my_bool)

  let mysql_dump_debug_info = foreign "mysql_dump_debug_info"
    (T.mysql @-> returning int)

  let mysql_set_server_option = foreign "mysql_set_server_option"
    (T.mysql @-> int @-> returning int)

  let mysql_ping = foreign "mysql_ping"
    (T.mysql @-> returning int)

  let mysql_stmt_prepare = foreign "mysql_stmt_prepare"
    (T.stmt @-> ptr char @-> ulong @-> returning int)

  let mysql_stmt_execute = foreign "mysql_stmt_execute"
    (T.stmt @-> returning int)

  let mysql_stmt_fetch = foreign "mysql_stmt_fetch"
    (T.stmt @-> returning int)

  let mysql_stmt_close = foreign "mysql_stmt_close"
    (T.stmt @-> returning T.my_bool)

  let mysql_stmt_store_result = foreign "mysql_stmt_store_result"
    (T.stmt @-> returning int)

  let mysql_stmt_free_result = foreign "mysql_stmt_free_result"
    (T.stmt @-> returning T.my_bool)

  (* Nonblocking API *)

  let mysql_close_start = foreign "mysql_close_start"
    (T.mysql @-> returning int)

  let mysql_close_cont = foreign "mysql_close_cont"
    (T.mysql @-> int @-> returning int)

  let mysql_real_connect_start = foreign "mysql_real_connect_start"
    (ptr T.mysql_opt @-> T.mysql @-> ptr_opt char @-> ptr_opt char @->
     ptr_opt char @-> ptr_opt char @-> uint @-> ptr_opt char @-> ulong @->
     returning int)

  let mysql_real_connect_cont = foreign "mysql_real_connect_cont"
    (ptr T.mysql_opt @-> T.mysql @-> int @-> returning int)

  let mysql_real_query_start = foreign "mysql_real_query_start"
    (ptr int @-> T.mysql @-> ptr char @-> ulong @-> returning int)

  let mysql_real_query_cont = foreign "mysql_real_query_cont"
    (ptr int @-> T.mysql @-> int @-> returning int)

  let mysql_fetch_row_start = foreign "mysql_fetch_row_start"
    (ptr T.row_opt @-> T.res @-> returning int)

  let mysql_fetch_row_cont = foreign "mysql_fetch_row_cont"
    (ptr T.row_opt @-> T.res @-> int @-> returning int)

  let mysql_free_result_start = foreign "mysql_free_result_start"
    (T.res @-> returning int)

  let mysql_free_result_cont = foreign "mysql_free_result_cont"
    (T.res @-> int @-> returning int)

  let mysql_get_socket = foreign "mysql_get_socket"
    (T.mysql @-> returning int)

  let mysql_get_timeout_value = foreign "mysql_get_timeout_value"
    (T.mysql @-> returning uint)

  let mysql_set_character_set_start = foreign "mysql_set_character_set_start"
    (ptr int @-> T.mysql @-> ptr char @-> returning int)

  let mysql_set_character_set_cont = foreign "mysql_set_character_set_cont"
    (ptr int @-> T.mysql @-> int @-> returning int)

  let mysql_select_db_start = foreign "mysql_select_db_start"
    (ptr int @-> T.mysql @-> ptr char @-> returning int)

  let mysql_select_db_cont = foreign "mysql_select_db_cont"
    (ptr int @-> T.mysql @-> int @-> returning int)

  let mysql_change_user_start = foreign "mysql_change_user_start"
    (ptr T.my_bool @-> T.mysql @-> ptr char @-> ptr char @-> ptr_opt char @->
     returning int)

  let mysql_change_user_cont = foreign "mysql_change_user_cont"
    (ptr T.my_bool @-> T.mysql @-> int @-> returning int)

  let mysql_dump_debug_info_start = foreign "mysql_dump_debug_info_start"
    (ptr int @-> T.mysql @-> returning int)

  let mysql_dump_debug_info_cont = foreign "mysql_dump_debug_info_cont"
    (ptr int @-> T.mysql @-> int @-> returning int)

  let mysql_set_server_option_start = foreign "mysql_set_server_option_start"
    (ptr int @-> T.mysql @-> int @-> returning int)

  let mysql_set_server_option_cont = foreign "mysql_set_server_option_cont"
    (ptr int @-> T.mysql @-> int @-> returning int)

  let mysql_ping_start = foreign "mysql_ping_start"
    (ptr int @-> T.mysql @-> returning int)

  let mysql_ping_cont = foreign "mysql_ping_cont"
    (ptr int @-> T.mysql @-> int @-> returning int)

  let mysql_stmt_prepare_start = foreign "mysql_stmt_prepare_start"
    (ptr int @-> T.stmt @-> ptr char @-> ulong @-> returning int)

  let mysql_stmt_prepare_cont = foreign "mysql_stmt_prepare_cont"
    (ptr int @-> T.stmt @-> int @-> returning int)

  let mysql_stmt_execute_start = foreign "mysql_stmt_execute_start"
    (ptr int @-> T.stmt @-> returning int)

  let mysql_stmt_execute_cont = foreign "mysql_stmt_execute_cont"
    (ptr int @-> T.stmt @-> int @-> returning int)

  let mysql_stmt_fetch_start = foreign "mysql_stmt_fetch_start"
    (ptr int @-> T.stmt @-> returning int)

  let mysql_stmt_fetch_cont = foreign "mysql_stmt_fetch_cont"
    (ptr int @-> T.stmt @-> int @-> returning int)

  let mysql_stmt_store_result_start = foreign "mysql_stmt_store_result_start"
    (ptr int @-> T.stmt @-> returning int)

  let mysql_stmt_store_result_cont = foreign "mysql_stmt_store_result_cont"
    (ptr int @-> T.stmt @-> int @-> returning int)

  let mysql_stmt_close_start = foreign "mysql_stmt_close_start"
    (ptr T.my_bool @-> T.stmt @-> returning int)

  let mysql_stmt_close_cont = foreign "mysql_stmt_close_cont"
    (ptr T.my_bool @-> T.stmt @-> int @-> returning int)

  let mysql_stmt_free_result_start = foreign "mysql_stmt_free_result_start"
    (ptr T.my_bool @-> T.stmt @-> returning int)

  let mysql_stmt_free_result_cont = foreign "mysql_stmt_free_result_cont"
    (ptr T.my_bool @-> T.stmt @-> int @-> returning int)

  let mysql_commit_start = foreign "mysql_commit_start"
    (ptr T.my_bool @-> T.mysql @-> returning int)

  let mysql_commit_cont = foreign "mysql_commit_cont"
    (ptr T.my_bool @-> T.mysql @-> int @-> returning int)

  let mysql_rollback_start = foreign "mysql_rollback_start"
    (ptr T.my_bool @-> T.mysql @-> returning int)

  let mysql_rollback_cont = foreign "mysql_rollback_cont"
    (ptr T.my_bool @-> T.mysql @-> int @-> returning int)

  let mysql_autocommit_start = foreign "mysql_autocommit_start"
    (ptr T.my_bool @-> T.mysql @-> T.my_bool @-> returning int)

  let mysql_autocommit_cont = foreign "mysql_autocommit_cont"
    (ptr T.my_bool @-> T.mysql @-> int @-> returning int)

  let mysql_stmt_next_result_start = foreign "mysql_stmt_next_result_start"
    (ptr int @-> T.stmt @-> returning int)

  let mysql_stmt_next_result_cont = foreign "mysql_stmt_next_result_cont"
    (ptr int @-> T.stmt @-> int @-> returning int)
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F
  include Foreign_bindings

  let handle (typ, z) f =
    let r = allocate typ z in
    let s = f r in
    (s, !@r)

  let handle_opt typ = handle (typ, None)
  let handle_int f = handle (int, 0) f
  let handle_char f = handle (char, '\000') f
  let handle_ret = handle_opt T.mysql_opt

  let mysql_init () =
    mysql_init None

  let mysql_options mysql opt value =
    mysql_options mysql opt value |> ignore

  let mysql_options4 mysql opt value1 value2 =
    mysql_options4 mysql opt value1 value2 |> ignore

  let mysql_stmt_attr_set_bool stmt attr value =
    let c = if value then '\001' else '\000' in
    let v = allocate T.my_bool c in
    mysql_stmt_attr_set stmt attr (to_voidp v) |> ignore

  let mysql_stmt_param_count stmt =
    Unsigned.ULong.to_int @@ mysql_stmt_param_count stmt

  let mysql_stmt_bind_param stmt bind =
    mysql_stmt_bind_param stmt (to_voidp bind) = '\000'

  let mysql_fetch_field_direct res i =
    mysql_fetch_field_direct res (Unsigned.UInt.of_int i)

  let mysql_stmt_bind_result stmt bind =
    mysql_stmt_bind_result stmt (to_voidp bind) = '\000'

  let mysql_stmt_num_rows stmt =
    Unsigned.ULLong.to_int @@ mysql_stmt_num_rows stmt

  let mysql_stmt_affected_rows stmt =
    Unsigned.ULLong.to_int @@ mysql_stmt_affected_rows stmt

  let char_ptr_buffer_of_string s =
    let len = String.length s in
    let buf = allocate_n char ~count:(len + 1) in
    for i = 0 to len - 1 do
      let p = buf +@ i in
      p <-@ s.[i]
    done;
    (buf +@ len) <-@ '\000';
    buf

  let char_ptr_opt_buffer_of_string = function
    | None -> None
    | Some s -> Some (char_ptr_buffer_of_string s)

  (* Blocking APi *)

  let mysql_real_connect mysql host user pass db port socket flags =
    let host = char_ptr_opt_buffer_of_string host in
    let user = char_ptr_opt_buffer_of_string user in
    let pass = char_ptr_opt_buffer_of_string pass in
    let db = char_ptr_opt_buffer_of_string db in
    let port = Unsigned.UInt.of_int port in
    let socket = char_ptr_opt_buffer_of_string socket in
    let flags = Unsigned.ULong.of_int flags in
    mysql_real_connect mysql host user pass db port socket flags

  let mysql_commit mysql =
    mysql_commit mysql = '\000'

  let mysql_rollback mysql =
    mysql_rollback mysql = '\000'

  let mysql_autocommit mysql auto =
    let auto = if auto then '\001' else '\000' in
    mysql_autocommit mysql auto = '\000'

  let mysql_set_character_set mysql charset =
    let charset = char_ptr_buffer_of_string charset in
    mysql_set_character_set mysql charset = 0

  let mysql_select_db mysql db =
    let db = char_ptr_buffer_of_string db in
    mysql_select_db mysql db = 0

  let mysql_change_user mysql user pass db =
    let user = char_ptr_buffer_of_string user in
    let pass = char_ptr_buffer_of_string pass in
    let db = char_ptr_opt_buffer_of_string db in
    mysql_change_user mysql user pass db = '\000'

  let mysql_dump_debug_info mysql =
    mysql_dump_debug_info mysql = 0

  let mysql_set_server_option mysql opt =
    mysql_set_server_option mysql opt = 0

  let mysql_ping mysql =
    mysql_ping mysql = 0

  let mysql_stmt_prepare stmt query =
    let len = Unsigned.ULong.of_int (String.length query) in
    let query = char_ptr_buffer_of_string query in
    mysql_stmt_prepare stmt query len = 0

  let mysql_stmt_execute stmt =
    mysql_stmt_execute stmt = 0

  let mysql_stmt_close stmt =
    mysql_stmt_close stmt = '\000'

  let mysql_stmt_store_result stmt =
    mysql_stmt_store_result stmt = 0

  let mysql_stmt_free_result stmt =
    mysql_stmt_free_result stmt = '\000'

  (* Nonblocking API *)

  let mysql_real_connect_start mysql host user pass db port socket flags =
    let host = char_ptr_opt_buffer_of_string host in
    let user = char_ptr_opt_buffer_of_string user in
    let pass = char_ptr_opt_buffer_of_string pass in
    let db = char_ptr_opt_buffer_of_string db in
    let port = Unsigned.UInt.of_int port in
    let socket = char_ptr_opt_buffer_of_string socket in
    let flags = Unsigned.ULong.of_int flags in
    handle_ret
      (fun ret ->
        mysql_real_connect_start ret mysql host user pass db port socket flags)

  let mysql_real_connect_cont mysql status =
    handle_ret (fun ret -> mysql_real_connect_cont ret mysql status)

  let mysql_real_query_start mysql query =
    let len = Unsigned.ULong.of_int (String.length query) in
    let query = char_ptr_buffer_of_string query in
    handle_int (fun err -> mysql_real_query_start err mysql query len)

  let mysql_real_query_cont mysql status =
    handle_int (fun err -> mysql_real_query_cont err mysql status)

  let mysql_get_timeout_value mysql =
    Unsigned.UInt.to_int @@ mysql_get_timeout_value mysql

  let mysql_set_character_set_start mysql charset =
    let charset = char_ptr_buffer_of_string charset in
    handle_int (fun ret -> mysql_set_character_set_start ret mysql charset)

  let mysql_set_character_set_cont mysql status =
    handle_int (fun ret -> mysql_set_character_set_cont ret mysql status)

  let mysql_select_db_start mysql db =
    let db = char_ptr_buffer_of_string db in
    handle_int (fun ret -> mysql_select_db_start ret mysql db)

  let mysql_select_db_cont mysql status =
    handle_int (fun ret -> mysql_select_db_cont ret mysql status)

  let mysql_change_user_start mysql user pass db =
    let user = char_ptr_buffer_of_string user in
    let pass = char_ptr_buffer_of_string pass in
    let db = char_ptr_opt_buffer_of_string db in
    handle_char (fun ret -> mysql_change_user_start ret mysql user pass db)

  let mysql_change_user_cont mysql status =
    handle_char (fun ret -> mysql_change_user_cont ret mysql status)

  let mysql_dump_debug_info_start mysql =
    handle_int (fun ret -> mysql_dump_debug_info_start ret mysql)

  let mysql_dump_debug_info_cont mysql status =
    handle_int (fun ret -> mysql_dump_debug_info_cont ret mysql status)

  let mysql_set_server_option_start mysql opt =
    handle_int (fun ret -> mysql_set_server_option_start ret mysql opt)

  let mysql_set_server_option_cont mysql status =
    handle_int (fun ret -> mysql_set_server_option_cont ret mysql status)

  let mysql_ping_start mysql =
    handle_int (fun ret -> mysql_ping_start ret mysql)

  let mysql_ping_cont mysql status =
    handle_int (fun ret -> mysql_ping_cont ret mysql status)

  let mysql_stmt_prepare_start stmt query =
    let len = Unsigned.ULong.of_int (String.length query) in
    let query = char_ptr_buffer_of_string query in
    handle_int (fun err -> mysql_stmt_prepare_start err stmt query len)

  let mysql_stmt_prepare_cont stmt status =
    handle_int (fun err -> mysql_stmt_prepare_cont err stmt status)

  let mysql_stmt_execute_start stmt =
    handle_int (fun err -> mysql_stmt_execute_start err stmt)

  let mysql_stmt_execute_cont stmt status =
    handle_int (fun err -> mysql_stmt_execute_cont err stmt status)

  let mysql_stmt_fetch_start stmt =
    handle_int (fun err -> mysql_stmt_fetch_start err stmt)

  let mysql_stmt_fetch_cont stmt status =
    handle_int (fun err -> mysql_stmt_fetch_cont err stmt status)

  let mysql_stmt_store_result_start stmt =
    handle_int (fun err -> mysql_stmt_store_result_start err stmt)

  let mysql_stmt_store_result_cont stmt status =
    handle_int (fun err -> mysql_stmt_store_result_cont err stmt status)

  let mysql_stmt_close_start stmt =
    handle_char (fun err -> mysql_stmt_close_start err stmt)

  let mysql_stmt_close_cont stmt status =
    handle_char (fun err -> mysql_stmt_close_cont err stmt status)

  let mysql_stmt_free_result_start stmt =
    handle_char (fun err -> mysql_stmt_free_result_start err stmt)

  let mysql_stmt_free_result_cont stmt status =
    handle_char (fun err -> mysql_stmt_free_result_cont err stmt status)

  let mysql_commit_start mysql =
    handle_char (fun err -> mysql_commit_start err mysql)

  let mysql_commit_cont mysql status =
    handle_char (fun err -> mysql_commit_cont err mysql status)

  let mysql_rollback_start mysql =
    handle_char (fun err -> mysql_rollback_start err mysql)

  let mysql_rollback_cont mysql status =
    handle_char (fun err -> mysql_rollback_cont err mysql status)

  let mysql_autocommit_start mysql auto =
    let auto = if auto then '\001' else '\000' in
    handle_char (fun err -> mysql_autocommit_start err mysql auto)

  let mysql_autocommit_cont mysql status =
    handle_char (fun err -> mysql_autocommit_cont err mysql status)

  let mysql_stmt_next_result_start stmt =
    handle_int (fun err -> mysql_stmt_next_result_start err stmt)

  let mysql_stmt_next_result_cont stmt status =
    handle_int (fun err -> mysql_stmt_next_result_cont err stmt status)
end
