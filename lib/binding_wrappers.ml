open Ctypes

module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

include B

let handle (typ, z) f =
  let r = allocate typ z in
  let s = f r in
  (s, !@r)

let handle_opt typ = handle (typ, None)
let handle_ret = handle_opt B.mysql_opt

let mysql_init () =
  B.mysql_init None

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
      B.mysql_real_connect_start ret mysql host user pass db port socket flags)

let mysql_real_connect_cont mysql status =
  handle_ret (fun ret -> B.mysql_real_connect_cont ret mysql status)

let mysql_get_timeout_value mysql =
  Unsigned.UInt.to_int @@ B.mysql_get_timeout_value mysql
