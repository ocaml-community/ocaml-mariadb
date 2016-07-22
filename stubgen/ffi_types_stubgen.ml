let () =
  print_endline "#include <mysql/mysql.h>";
  Cstubs.Types.write_c Format.std_formatter (module Ffi_bindings.Types)
