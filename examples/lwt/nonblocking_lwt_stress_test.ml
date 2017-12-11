open Lwt.Infix

module S = Mariadb.Nonblocking.Status

module Test = Nonblocking_stress_test.Make (struct

  module IO = struct
    type 'a future = 'a Lwt.t
    let (>>=) = (>>=)
    let return = Lwt.return
  end

  let wait mariadb status =
    let fd = Lwt_unix.of_unix_file_descr @@ Mariadb.Nonblocking.fd mariadb in
    assert (S.read status || S.write status || S.timeout status);
    let idle, _ = Lwt.task () in
    let rt = if S.read status then Lwt_unix.wait_read fd else idle in
    let wt = if S.write status then Lwt_unix.wait_write fd else idle in
    let tt =
      match S.timeout status, Mariadb.Nonblocking.timeout mariadb with
      | true, 0 -> Lwt.return ()
      | true, tmout -> Lwt_unix.timeout (float tmout)
      | false, _ -> idle in
    Lwt.catch
      (fun () ->
        Lwt.nchoose [rt; wt; tt] >>= fun _ ->
        Lwt.return @@
          S.create
            ~read:(Lwt_unix.readable fd)
            ~write:(Lwt_unix.writable fd)
            ())
      (function
       | Lwt_unix.Timeout -> Lwt.return @@ S.create ~timeout:true ()
       | e -> Lwt.fail e)
end)

let () = Lwt_main.run (Test.main ())
