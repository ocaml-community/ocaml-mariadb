open Ctypes

module Types (F: Cstubs.Types.TYPE) = struct
  open F
  module Options = struct
    let nonblock = constant "MYSQL_OPT_NONBLOCK" int
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
    let buffer_length = field t "buffer_length" ulong
    let buffer_type = field t "buffer_type" int
    let is_unsigned = field t "is_unsigned" char

    let () = seal t
  end

  module Field_flags = struct
    let unsigned = constant "UNSIGNED_FLAG" int
  end

  module Field = struct
    type field
    type t = field structure
    let t : t typ = structure "st_mysql_field"

    let name = field t "name" string
    let max_length = field t "max_length" ulong
    let flags = field t "flags" uint
    let type_ = field t "type" int

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

  let mysql_init = foreign "mysql_init"
    (T.mysql_opt @-> returning T.mysql_opt)

  let mysql_close = foreign "mysql_close"
    (T.mysql @-> returning void)

  let mysql_options = foreign "mysql_options"
    (T.mysql @-> int @-> ptr void @-> returning int)

  let mysql_use_result = foreign "mysql_use_result"
    (T.mysql @-> returning T.res_opt)

  let mysql_free_result = foreign "mysql_free_result"
    (T.res @-> returning void)

  let mysql_num_fields = foreign "mysql_num_fields"
    (T.res @-> returning int)

  let mysql_num_rows = foreign "mysql_num_rows"
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

  (* Nonblocking API *)

  let mysql_close_start = foreign "mysql_close_start"
    (T.mysql @-> returning int)

  let mysql_close_cont = foreign "mysql_close_cont"
    (T.mysql @-> int @-> returning int)

  let mysql_real_connect_start = foreign "mysql_real_connect_start"
    (ptr T.mysql_opt @-> T.mysql @-> string_opt @-> string_opt @->
     string_opt @-> string_opt @-> uint @-> string_opt @-> ulong @->
     returning int)

  let mysql_real_connect_cont = foreign "mysql_real_connect_cont"
    (ptr T.mysql_opt @-> T.mysql @-> int @-> returning int)

  let mysql_real_query_start = foreign "mysql_real_query_start"
    (ptr int @-> T.mysql @-> string @-> ulong @-> returning int)

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
    (ptr T.mysql_opt @-> T.mysql @-> string @-> returning int)

  let mysql_set_character_set_cont = foreign "mysql_set_character_set_cont"
    (ptr T.mysql_opt @-> T.mysql @-> int @-> returning int)

  let mysql_select_db_start = foreign "mysql_select_db_start"
    (ptr T.mysql_opt @-> T.mysql @-> string @-> returning int)

  let mysql_select_db_cont = foreign "mysql_select_db_cont"
    (ptr T.mysql_opt @-> T.mysql @-> int @-> returning int)

  let mysql_change_user_start = foreign "mysql_change_user_start"
    (ptr T.mysql_opt @-> T.mysql @-> string @-> string @-> string_opt @->
     returning int)

  let mysql_change_user_cont = foreign "mysql_change_user_cont"
    (ptr T.mysql_opt @-> T.mysql @-> int @-> returning int)

  let mysql_dump_debug_info_start = foreign "mysql_dump_debug_info_start"
    (ptr T.mysql_opt @-> T.mysql @-> returning int)

  let mysql_dump_debug_info_cont = foreign "mysql_dump_debug_info_cont"
    (ptr T.mysql_opt @-> T.mysql @-> int @-> returning int)

  let mysql_set_server_option_start = foreign "mysql_set_server_option_start"
    (ptr T.mysql_opt @-> T.mysql @-> int @-> returning int)

  let mysql_set_server_option_cont = foreign "mysql_set_server_option_cont"
    (ptr T.mysql_opt @-> T.mysql @-> int @-> returning int)

  let mysql_ping_start = foreign "mysql_ping_start"
    (ptr T.mysql_opt @-> T.mysql @-> returning int)

  let mysql_ping_cont = foreign "mysql_ping_cont"
    (ptr T.mysql_opt @-> T.mysql @-> int @-> returning int)

  let mysql_list_dbs_start = foreign "mysql_list_dbs_start"
    (ptr T.res_opt @-> T.mysql @-> string @-> returning int)

  let mysql_list_dbs_cont = foreign "mysql_list_dbs_cont"
    (ptr T.res_opt @-> T.mysql @-> int @-> returning int)

  let mysql_list_tables_start = foreign "mysql_list_tables_start"
    (ptr T.res_opt @-> T.mysql @-> string @-> returning int)

  let mysql_list_tables_cont = foreign "mysql_list_tables_cont"
    (ptr T.res_opt @-> T.mysql @-> int @-> returning int)

  let mysql_stmt_prepare_start = foreign "mysql_stmt_prepare_start"
    (ptr int @-> T.stmt @-> string @-> ulong @-> returning int)

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

  let mysql_stmt_reset_start = foreign "mysql_stmt_reset_start"
    (ptr T.my_bool @-> T.stmt @-> returning int)

  let mysql_stmt_reset_cont = foreign "mysql_stmt_reset_cont"
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

  let mysql_next_result_start = foreign "mysql_next_result_start"
    (ptr int @-> T.mysql @-> returning int)

  let mysql_next_result_cont = foreign "mysql_next_result_cont"
    (ptr int @-> T.mysql @-> int @-> returning int)

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
  let handle_res = handle_opt T.res_opt

  let mysql_init () =
    mysql_init None

  let mysql_options mysql opt value =
    mysql_options mysql opt value |> ignore

  let mysql_stmt_attr_set_bool stmt attr value =
    let c = if value then '\001' else '\000' in
    let v = allocate T.my_bool c in
    mysql_stmt_attr_set stmt attr (to_voidp v) |> ignore

  let mysql_stmt_param_count stmt =
    Unsigned.ULong.to_int @@ mysql_stmt_param_count stmt

  let mysql_stmt_bind_param stmt bind =
    match mysql_stmt_bind_param stmt (to_voidp bind) with
    | '\000' -> true
    | _ -> false

  let mysql_fetch_field_direct res i =
    mysql_fetch_field_direct res (Unsigned.UInt.of_int i)

  let mysql_stmt_bind_result stmt bind =
    match mysql_stmt_bind_result stmt (to_voidp bind) with
    | '\000' -> true
    | _ -> false

  let mysql_stmt_num_rows stmt =
    Unsigned.ULLong.to_int @@ mysql_stmt_num_rows stmt

  (* Nonblocking API *)

  let mysql_real_connect_start mysql host user pass db port socket flags =
    let port = Unsigned.UInt.of_int port in
    let flags = Unsigned.ULong.of_int flags in
    handle_ret
      (fun ret ->
        mysql_real_connect_start ret mysql host user pass db port socket flags)

  let mysql_real_connect_cont mysql status =
    handle_ret (fun ret -> mysql_real_connect_cont ret mysql status)

  let mysql_real_query_start mysql query =
    let len = Unsigned.ULong.of_int (String.length query) in
    handle_int (fun err -> mysql_real_query_start err mysql query len)

  let mysql_real_query_cont mysql status =
    handle_int (fun err -> mysql_real_query_cont err mysql status)

  let string_of_char_ptr p =
    let b = Buffer.create 256 in
    let continue = ref true in
    let i = ref 0 in
    while !continue do
      let c = !@(p +@ !i) in
      if c = '\x00' then
        continue := false
      else
        Buffer.add_char b c;
      incr i
    done;
    Buffer.contents b

  let make_array pp len =
    let a = Array.make len "" in
    let p = ref (!@pp) in
    for i = 0 to len - 1 do
      let s = string_of_char_ptr !p in
      a.(i) <- s;
      p := !p +@ (String.length s + 1)
    done;
    a

  let fetch_row res f =
    let row = allocate T.row_opt None in
    let status = f row in
    let num = mysql_num_fields res in
    match !@row with
    | Some r -> (status, Some (make_array r num))
    | None -> (status, None)

  let mysql_fetch_row_start res =
    fetch_row res (fun row -> mysql_fetch_row_start row res)

  let mysql_fetch_row_cont res status =
    fetch_row res (fun row -> mysql_fetch_row_cont row res status)

  let mysql_get_timeout_value mysql =
    Unsigned.UInt.to_int @@ mysql_get_timeout_value mysql

  let mysql_set_character_set_start mysql charset =
    handle_ret (fun ret -> mysql_set_character_set_start ret mysql charset)

  let mysql_set_character_set_cont mysql status =
    handle_ret (fun ret -> mysql_set_character_set_cont ret mysql status)

  let mysql_select_db_start mysql db =
    handle_ret (fun ret -> mysql_select_db_start ret mysql db)

  let mysql_select_db_cont mysql status =
    handle_ret (fun ret -> mysql_select_db_cont ret mysql status)

  let mysql_change_user_start mysql user pass db =
    handle_ret (fun ret -> mysql_change_user_start ret mysql user pass db)

  let mysql_change_user_cont mysql status =
    handle_ret (fun ret -> mysql_change_user_cont ret mysql status)

  let mysql_dump_debug_info_start mysql =
    handle_ret (fun ret -> mysql_dump_debug_info_start ret mysql)

  let mysql_dump_debug_info_cont mysql status =
    handle_ret (fun ret -> mysql_dump_debug_info_cont ret mysql status)

  let mysql_set_server_option_start mysql opt =
    handle_ret (fun ret -> mysql_set_server_option_start ret mysql opt)

  let mysql_set_server_option_cont mysql status =
    handle_ret (fun ret -> mysql_set_server_option_cont ret mysql status)

  let mysql_ping_start mysql =
    handle_ret (fun ret -> mysql_ping_start ret mysql)

  let mysql_ping_cont mysql status =
    handle_ret (fun ret -> mysql_ping_cont ret mysql status)

  let mysql_list_dbs_start mysql wild =
    handle_res (fun res -> mysql_list_dbs_start res mysql wild)

  let mysql_list_dbs_cont mysql status =
    handle_res (fun res -> mysql_list_dbs_cont res mysql status)

  let mysql_list_tables_start mysql wild =
    handle_res (fun res -> mysql_list_tables_start res mysql wild)

  let mysql_list_tables_cont mysql status =
    handle_res (fun res -> mysql_list_tables_cont res mysql status)

  let mysql_stmt_prepare_start stmt query =
    let len = Unsigned.ULong.of_int (String.length query) in
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

  let mysql_stmt_reset_start stmt =
    handle_char (fun err -> mysql_stmt_reset_start err stmt)

  let mysql_stmt_reset_cont stmt status =
    handle_char (fun err -> mysql_stmt_reset_cont err stmt status)

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

  let mysql_next_result_start mysql =
    handle_int (fun err -> mysql_next_result_start err mysql)

  let mysql_next_result_cont mysql status =
    handle_int (fun err -> mysql_next_result_cont err mysql status)

  let mysql_stmt_next_result_start stmt =
    handle_int (fun err -> mysql_stmt_next_result_start err stmt)

  let mysql_stmt_next_result_cont stmt status =
    handle_int (fun err -> mysql_stmt_next_result_cont err stmt status)
end
