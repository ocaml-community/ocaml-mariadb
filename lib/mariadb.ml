module B = Ffi_bindings.Bindings(Ffi_generated)

module Common = Common
module Blocking = Blocking
module Nonblocking = Nonblocking

type error = int * string

let () =
  match B.mysql_library_init 0 None None with
  | 0 -> at_exit B.mysql_library_end
  | _ -> failwith "cannot initialize MariaDB library"
