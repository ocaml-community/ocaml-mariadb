open Ctypes

module Types (F: Cstubs.Types.TYPE) = struct
  open F

  module Options = struct
    let nonblock = constant "MYSQL_OPT_NONBLOCK" int
  end

  module Wait_status = struct
    let read = constant "MYSQL_WAIT_READ" int
    let write = constant "MYSQL_WAIT_WRITE" int
    let except = constant "MYSQL_WAIT_EXCEPT" int
    let timeout = constant "MYSQL_WAIT_TIMEOUT" int
  end
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

  type mysql = unit ptr
  let mysql : mysql typ = ptr void

  type mysql_opt = unit ptr option
  let mysql_opt : mysql_opt typ = ptr_opt void

  let mysql_library_init = foreign "mysql_server_init"
    (int @-> ptr_opt (ptr char) @-> ptr_opt (ptr char) @-> returning int)

  let mysql_init = foreign "mysql_init"
    (mysql_opt @-> returning mysql_opt)

  let mysql_close_start = foreign "mysql_close_start"
    (mysql @-> returning int)

  let mysql_close_cont = foreign "mysql_close_cont"
    (mysql @-> int @-> returning int)

  let mysql_real_connect_start = foreign "mysql_real_connect_start"
    (ptr mysql_opt @-> mysql @-> ptr_opt char @-> ptr_opt char @->
     ptr_opt char @-> ptr_opt char @-> uint @-> ptr_opt char @-> ulong @->
     returning int)

  let mysql_real_connect_cont = foreign "mysql_real_connect_cont"
    (ptr mysql_opt @-> mysql @-> int @-> returning int)

  let mysql_options = foreign "mysql_options"
    (mysql @-> int @-> ptr void @-> returning int)

  let mysql_get_socket = foreign "mysql_get_socket"
    (mysql @-> returning int)

  let mysql_errno = foreign "mysql_errno"
    (mysql @-> returning int)

  let mysql_error = foreign "mysql_error"
    (mysql @-> returning string)

  let mysql_get_timeout_value = foreign "mysql_get_timeout_value"
    (mysql @-> returning uint)
end
