let flip f = fun x y -> f y x

let char_ptr_buffer_of_string s =
  let open Ctypes in
  let len = String.length s in
  let buf = allocate_n char ~count:(len + 1) in
  for i = 0 to len - 1 do
    let p = buf +@ i in
    p <-@ s.[i]
  done;
  (buf +@ len) <-@ '\000';
  buf
