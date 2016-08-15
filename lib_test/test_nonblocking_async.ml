module Caml_bytes = Bytes
open Core.Std
open Async.Std
open Print

module S = Mariadb.Nonblocking.Status
module M = Mariadb.Nonblocking.Make(struct
  module IO = struct
    type 'a future = 'a Deferred.t
    let (>>=) = (>>=)
    let return = Deferred.return
  end

  let is_ready = function
    | `Ready -> true
    | `Bad_fd | `Closed -> false

  let ready (rt, wt, tt) =
    let r = ref false in
    let w = ref false in
    let t = ref false in
    let rc = Deferred.choice rt (fun x -> r := is_ready x) in
    let wc = Deferred.choice wt (fun x -> w := is_ready x) in
    let tc = Deferred.choice tt (fun x -> t := true) in
    Deferred.enabled [rc; wc; tc] >>= fun f ->
    ignore (f ());
    Deferred.return (!r, !w, !t)

  let wait mariadb status =
    let fd =
      Fd.create
        (Fd.Kind.Socket `Active)
        (Mariadb.Nonblocking.fd mariadb)
        (Info.of_string "<mariadb fd>") in
    assert (S.read status || S.write status || S.timeout status);
    let idle = Deferred.never () in
    let rt =
      if S.read status then Fd.ready_to fd `Read
      else idle in
    let wt =
      if S.write status then begin Fd.ready_to fd `Write
      end
      else idle in
    let tt =
      let tmout = float (Mariadb.Nonblocking.timeout mariadb) in
      if S.timeout status then Clock.after (Time.Span.of_sec tmout)
      else idle in
    ready (rt, wt, tt) >>= fun (read, write, timeout) ->
    Fd.close ~should_close_file_descriptor:false fd >>= fun () ->
    Deferred.return @@ S.create ~read ~write ~timeout ()
end)

let env var def =
  match Sys.getenv var with
  | Some v -> v
  | None -> def

let or_die ?(info = "error") = function
  | Pervasives.Ok r -> return r
  | Pervasives.Error (i, e) -> failwith @@ sprintf "%s: (%d) %s" info i e

let print_row row =
  printf "---\n%!";
  M.Row.StringMap.fold
    (fun name field _ ->
      printf "%20s " name;
      match M.Field.value field with
      | `Int i -> printf "%d\n%!" i
      | `Float x -> printf "%f\n%!" x
      | `String s -> printf "%s\n%!" s
      | `Bytes b -> printf "%s\n%!" (Caml_bytes.to_string b)
      | `Time t ->
          printf "%04d-%02d-%02d %02d:%02d:%02d\n%!"
            (M.Time.year t)
            (M.Time.month t)
            (M.Time.day t)
            (M.Time.hour t)
            (M.Time.minute t)
            (M.Time.second t)
      | `Null -> printf "NULL\n%!")
    row
    ();
  return ()

let connect () =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~pass:(env "OCAML_MARIADB_PASS" "")
    ~db:(env "OCAML_MARIADB_DB" "mysql") ()

let stream res =
  let build _ =
    M.Res.fetch (module M.Row.Map) res
    >>| function
      | Ok (Some row) -> Some (row, row)
      | Ok None | Error _ -> None in
  M.Res.fetch (module M.Row.Map) res
  >>| function
    | Ok (Some init) -> Some (Pipe.unfold ~init ~f:build)
    | Ok None | Error _ -> None

let print_rows = function
  | Some p -> Pipe.iter p ~f:print_row
  | None -> return ()

let main =
  connect () >>= or_die ~info:"connect" >>= fun mariadb ->
  let query = env "OCAML_MARIADB_QUERY"
    "SELECT * FROM user WHERE LENGTH(user) > ?" in
  M.prepare mariadb query >>= or_die ~info:"prepare" >>= fun stmt ->
  M.Stmt.execute stmt [| `String "Problema%" |] >>= or_die >>= fun res ->
  printf "#rows: %d\n%!" (M.Res.num_rows res);
  stream res >>= fun p ->
  print_rows p >>= function () ->
  M.Stmt.close stmt >>= or_die >>= fun () ->
  M.close mariadb >>= fun () ->
  Shutdown.exit 0

let () = never_returns (Scheduler.go ())
