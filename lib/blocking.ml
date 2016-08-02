open Util

module B = Ffi_bindings.Bindings(Ffi_generated)

module Stmt = Common.Stmt
module Res = Common.Res

type 's t = ([`Blocking], 's) Common.t

let init () =
  B.mysql_init ()

let close =
  B.mysql_close
