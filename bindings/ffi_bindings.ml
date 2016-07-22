open Ctypes

module Types (F: Cstubs.Types.TYPE) = struct
  open F
  module Mariadb_options = struct
    let nonblock = constant "MYSQL_OPT_NONBLOCK" int
  end
  module Mariadb_wait_status = struct
    let read = constant "MYSQL_WAIT_READ" int
    let write = constant "MYSQL_WAIT_WRITE" int
    let except = constant "MYSQL_WAIT_EXCEPT" int
    let timeout = constant "MYSQL_WAIT_TIMEOUT" int
  end
end

module Foreign_bindings = struct
  open Foreign

  let foreign name typ = foreign name typ
    ~from:Dl.(dlopen ~filename:"libmysqlclient.so" ~flags:[RTLD_NOW])

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

  let mysql_init = foreign "mysql_init"
    (mysql_opt @-> returning mysql_opt)

  let mysql_close = foreign "mysql_close"
    (mysql @-> returning void)

  let mysql_options = foreign "mysql_options"
    (mysql @-> int @-> ptr void @-> returning int)

  let mysql_use_result = foreign "mysql_use_result"
    (mysql @-> returning res_opt)

  let mysql_free_result = foreign "mysql_free_result"
    (res @-> returning void)

  let mysql_num_fields = foreign "mysql_num_fields"
    (res @-> returning int)

  let mysql_num_rows = foreign "mysql_num_rows"
    (res @-> returning int)

  let mysql_errno = foreign "mysql_errno"
    (mysql @-> returning int)

  let mysql_error = foreign "mysql_error"
    (mysql @-> returning string)

  (* Nonblocking API *)

  let mysql_close_start = foreign "mysql_close_start"
    (mysql @-> returning int)

  let mysql_close_cont = foreign "mysql_close_cont"
    (mysql @-> int @-> returning int)

  let mysql_real_connect_start = foreign "mysql_real_connect_start"
    (ptr mysql_opt @-> mysql @-> string_opt @-> string_opt @-> string_opt @->
     string_opt @-> uint @-> string_opt @-> ulong @-> returning int)

  let mysql_real_connect_cont = foreign "mysql_real_connect_cont"
    (ptr mysql_opt @-> mysql @-> int @-> returning int)

  let mysql_real_query_start = foreign "mysql_real_query_start"
    (ptr int @-> mysql @-> string @-> ulong @-> returning int)

  let mysql_real_query_cont = foreign "mysql_real_query_cont"
    (ptr int @-> mysql @-> int @-> returning int)

  let mysql_fetch_row_start = foreign "mysql_fetch_row_start"
    (ptr row_opt @-> res @-> returning int)

  let mysql_fetch_row_cont = foreign "mysql_fetch_row_cont"
    (ptr row_opt @-> res @-> int @-> returning int)

  let mysql_free_result_start = foreign "mysql_free_result_start"
    (res @-> returning int)

  let mysql_free_result_cont = foreign "mysql_free_result_start"
    (res @-> int @-> returning int)

  let mysql_get_socket = foreign "mysql_get_socket"
    (mysql @-> returning int)

  let mysql_get_timeout_value = foreign "mysql_get_timeout_value"
    (mysql @-> returning uint)
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F
  include Foreign_bindings

  let mysql_init ?(conn = None) () =
    mysql_init conn

  let mysql_options conn opt value =
    mysql_options conn opt value |> ignore

  (* Nonblocking API *)

  let mysql_real_connect_start conn host user pass db port socket flags =
    let port = Unsigned.UInt.of_int port in
    let flags = Unsigned.ULong.of_int flags in
    let ret = allocate mysql_opt None in
    let status =
      mysql_real_connect_start ret conn host user pass db port socket flags in
    (status, !@ret)

  let mysql_real_connect_cont conn status =
    let ret = allocate mysql_opt None in
    let status = mysql_real_connect_cont ret conn status in
    (status, !@ret)

  let mysql_real_query_start conn query =
    let err = allocate int 0 in
    let len = Unsigned.ULong.of_int (String.length query) in
    let status = mysql_real_query_start err conn query len in
    (status, !@err)

  let mysql_real_query_cont conn status =
    let err = allocate int 0 in
    let status = mysql_real_query_cont err conn status in
    (status, !@err)

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
    let row = allocate row_opt None in
    let status = f row in
    let num = mysql_num_fields res in
    match !@row with
    | Some r -> (status, Some (make_array r num))
    | None -> (status, None)

  let mysql_fetch_row_start res =
    fetch_row res (fun row -> mysql_fetch_row_start row res)

  let mysql_fetch_row_cont res status =
    fetch_row res (fun row -> mysql_fetch_row_cont row res status)

  let mysql_get_timeout_value conn =
    Unsigned.UInt.to_int @@ mysql_get_timeout_value conn
end
