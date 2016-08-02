open Util

module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

module Status = Wait_status
module Error = Common.Error

type 's t = ([`Nonblocking], 's) Common.t
type 's mariadb = 's t

type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Error.t]

type server_option = Common.server_option

type 'a start = unit -> 'a result
type 'a cont = Status.t -> 'a result
type 'a nonblocking = 'a start * 'a cont

let map_result f = function
  | `Ok x -> `Ok (f x)
  | `Wait _ as w -> w
  | `Error _ as e -> e

type options =
  | Nonblocking

let options mariadb = function
  | Nonblocking ->
      B.mysql_options mariadb T.Options.nonblock Ctypes.null

let init () =
  match B.mysql_init () with
  | Some raw ->
      options raw Nonblocking;
      Some raw
  | None ->
      None

let handle_opt mariadb f =
  match f mariadb with
  | 0, Some r -> `Ok r
  | 0, None -> `Error (Error.create mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_unit mariadb f =
  match handle_opt mariadb f with
  | `Ok _ -> `Ok ()
  | `Wait s -> `Wait s
  | `Error e -> `Error e

let handle_int_ret mariadb f =
  match f mariadb with
  | 0, 0 -> `Ok mariadb
  | 0, _ -> `Error (Error.create mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_int mariadb f =
  match f mariadb with
  | 0, 0 -> `Ok ()
  | 0, _ -> `Error (Error.create mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_char mariadb f =
  match f mariadb with
  | 0, '\000' -> `Ok ()
  | 0, _ -> `Error (Error.create mariadb)
  | s, _ -> `Wait (Status.of_int s)

let connect_start mariadb host user pass db port socket flags () =
  handle_opt mariadb
    (fun m ->
      B.mysql_real_connect_start m host user pass db port socket flags)

let connect_cont mariadb status =
  handle_opt mariadb
    (fun m -> B.mysql_real_connect_cont m (Status.to_int status))

let connect mariadb ?host ?user ?pass ?db ?(port=0) ?socket ?(flags=[]) () =
  (* TODO flags *)
  let start = connect_start mariadb host user pass db port socket 0 in
  let cont = connect_cont mariadb in
  (start, cont)

let handle_ok_wait mariadb f =
  match f mariadb with
  | 0 -> `Ok
  | s -> `Wait (Status.of_int s)

let close_start mariadb () =
  handle_ok_wait mariadb (fun raw -> B.mysql_close_start raw)

let close_cont mariadb status =
  handle_ok_wait mariadb (fun raw -> B.mysql_close_cont raw status)

let close mariadb =
  (close_start mariadb, close_cont mariadb)

let fd =
  B.mysql_get_socket

let timeout =
  B.mysql_get_timeout_value

let set_charset_start mariadb charset () =
  handle_unit mariadb ((flip B.mysql_set_character_set_start) charset)

let set_charset_cont mariadb status =
  handle_unit mariadb ((flip B.mysql_set_character_set_cont) status)

let set_charset mariadb charset =
  (set_charset_start mariadb charset, set_charset_cont mariadb)

let select_db_start mariadb db () =
  handle_unit mariadb ((flip B.mysql_select_db_start) db)

let select_db_cont mariadb status =
  handle_unit mariadb ((flip B.mysql_select_db_cont) status)

let select_db mariadb db =
  (select_db_start mariadb db, select_db_cont mariadb)

let change_user_start mariadb user pass db () =
  handle_unit mariadb (fun m -> B.mysql_change_user_start m user pass db)

let change_user_cont mariadb status =
  handle_unit mariadb ((flip B.mysql_change_user_cont) status)

let change_user mariadb user pass db =
  (change_user_start mariadb user pass db, change_user_cont mariadb)

let dump_debug_info_start mariadb () =
  handle_unit mariadb B.mysql_dump_debug_info_start

let dump_debug_info_cont mariadb status =
  handle_unit mariadb ((flip B.mysql_dump_debug_info_cont) status)

let dump_debug_info mariadb =
  (dump_debug_info_start mariadb, dump_debug_info_cont mariadb)

let set_server_option_start mariadb opt () =
  let opt =
    match opt with
    | Common.Multi_statements true -> T.Server_options.multi_statements_on
    | Common.Multi_statements false -> T.Server_options.multi_statements_off in
  handle_unit mariadb ((flip B.mysql_set_server_option_start) opt)

let set_server_option_cont mariadb status =
  handle_unit mariadb ((flip B.mysql_set_server_option_cont) status)

let set_server_option mariadb opt =
  (set_server_option_start mariadb opt, set_server_option_cont mariadb)

let ping_start mariadb () =
  handle_unit mariadb B.mysql_ping_start

let ping_cont mariadb status =
  handle_unit mariadb ((flip B.mysql_ping_cont) status)

let ping mariadb =
  (ping_start mariadb, ping_cont mariadb)

(*let list_dbs_start mariadb wild =
  handle_opt mariadb (fun m -> B.mysql_list_dbs_start m wild)

let list_dbs_cont mariadb status =
handle_opt mariadb (fun m -> B.mysql_list_dbs_cont m status)

let list_tables_start mariadb wild =
handle_opt mariadb (fun m -> B.mysql_list_tables_start m wild)

let list_tables_cont mariadb status =
  handle_opt mariadb (fun m -> B.mysql_list_tables_cont m status)

let handle_next mariadb f errf =
  match f mariadb with
  | 0, 0 -> `Ok true
  | 0, -1 -> `Ok false
  | 0, _ -> `Error (Error.create mariadb)
  | s, _ -> `Wait (Status.of_int s)

let next_result_start mariadb =
  handle_next mariadb (fun m -> B.mysql_next_result_start m)

let next_result_cont mariadb status =
  handle_next mariadb (fun m -> B.mysql_next_result_cont m status)*)

let build_stmt mariadb raw =
  match Common.Stmt.init mariadb raw with
  | Some stmt -> `Ok stmt
  | None -> `Error (Error.create mariadb)

let handle_prepare mariadb raw f =
  match f raw with
  | 0, 0 -> build_stmt mariadb raw
  | 0, _ -> `Error (B.mysql_stmt_errno raw, B.mysql_stmt_error raw)
  | s, _ -> `Wait (Status.of_int s)

let prepare_start mariadb raw_stmt query () =
  handle_prepare mariadb raw_stmt ((flip B.mysql_stmt_prepare_start) query)

let prepare_cont mariadb raw_stmt status =
  handle_prepare mariadb raw_stmt ((flip B.mysql_stmt_prepare_cont) status)

let prepare mariadb query =
  match Common.stmt_init mariadb with
  | Some raw ->
      `Ok (prepare_start mariadb raw query, prepare_cont mariadb raw)
  | None ->
      `Error (Error.create mariadb)

module Res = struct
  type t = [`Nonblocking] Common.Res.t

  type time = Common.Res.time =
    { year : int
    ; month : int
    ; day : int
    ; hour : int
    ; minute : int
    ; second : int
    }

  let handle_fetch_row res f =
    match f res.Common.Res.raw with
    | 0, Some row -> `Ok (Some row)
    | 0, None -> `Ok (None)
    | s, _ -> `Wait (Status.of_int s)

  let fetch_row_start res () =
    handle_fetch_row res B.mysql_fetch_row_start

  let fetch_row_cont res status =
    handle_fetch_row res ((flip B.mysql_fetch_row_cont) status)

  let fetch_row res =
    (fetch_row_start res, fetch_row_cont res)

  let buffer_of_char_ptr p len =
    let b = Buffer.create len in
    let i = ref 0 in
    let open Ctypes in
    while !i < len do
      let c = !@(p +@ !i) in
      Buffer.add_char b c;
      incr i
    done;
    b

  let convert r at typ =
    let open Ctypes in
    let bp = r.Common.Bind.bind +@ at in
    let buf = getf (!@bp) T.Bind.buffer in
    let cast_to t =
      !@(coerce (ptr void) (ptr t) buf) in
    let to_char_buffer () =
      let lp = r.Common.Bind.length +@ at in
      let len = Unsigned.ULong.to_int @@ !@lp in
      buffer_of_char_ptr (coerce (ptr void) (ptr char) buf) len in
    match typ with
    | `Null ->
        `Null
    | `Tiny | `Year ->
        `Int (int_of_char @@ cast_to char)
    | `Short ->
        `Int (cast_to int)
    | `Int24 | `Long ->
        `Int (Signed.Int32.to_int @@ cast_to int32_t)
    | `Long_long ->
        `Int (Signed.Int64.to_int @@ cast_to int64_t)
    | `Float ->
        `Float (cast_to float)
    | `Double ->
        `Float (cast_to double)
    | `Decimal | `New_decimal | `String | `Var_string | `Bit ->
        `String (to_char_buffer () |> Buffer.contents)
    | `Tiny_blob | `Blob | `Medium_blob | `Long_blob ->
        `Bytes (to_char_buffer () |> Buffer.to_bytes)
    | `Time  | `Date | `Datetime | `Timestamp ->
        let tp = coerce (ptr void) (ptr T.Time.t) buf in
        let field f = Unsigned.UInt.to_int @@ getf (!@tp) f in
        `Time
          { Common.Res.
            year   = field T.Time.year
          ; month  = field T.Time.month
          ; day    = field T.Time.day
          ; hour   = field T.Time.hour
          ; minute = field T.Time.minute
          ; second = field T.Time.second
          }

  let build_row res =
    let r = res.Common.Res.result in
    let n = r.Common.Bind.n in
    let open Ctypes in
    Array.init n
      (fun i ->
        let bp = r.Common.Bind.bind +@ i in
        (* TODO test is_null *)
        let buffer_type = getf (!@bp) T.Bind.buffer_type in
        (* TODO test is_unsigned *)
        convert r i (Common.Bind.buffer_type_of_int buffer_type))

  let handle_fetch res f =
    match f res.Common.Res.stmt with
    | 0, 0 -> `Ok (Some (build_row res))
    | 0, 1 ->
        let errno = B.mysql_stmt_errno res.Common.Res.stmt in
        let error = B.mysql_stmt_error res.Common.Res.stmt in
        `Error (Common.Stmt.Error.make errno error)
    | 0, r when r = T.Return_code.no_data -> `Ok None
    | 0, r when r = T.Return_code.data_truncated ->
        `Error (Common.Stmt.Error.make 0 "truncated data")
    | s, _ -> `Wait (Status.of_int s)

  let fetch_start res () =
    handle_fetch res B.mysql_stmt_fetch_start

  let fetch_cont stmt status =
    handle_fetch stmt ((flip B.mysql_stmt_fetch_cont) status)

  let fetch res =
    (fetch_start res, fetch_cont res)

  let handle_ok_wait res f =
    match f res.Common.Res.raw with
    | 0 -> `Ok
    | s -> `Wait (Status.of_int s)

  let free_start res () =
    handle_ok_wait res B.mysql_free_result_start

  let free_cont res status =
    handle_ok_wait res ((flip B.mysql_free_result_cont) status)

  let free res =
    (free_start res, free_cont res)

  let num_rows =
    Common.Res.num_rows
end

module Stmt = struct
  module Error = Common.Stmt.Error

  type 's t = ([`Nonblocking], 's) Common.Stmt.t

  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Error.t]

  let init =
    Common.Stmt.init

  let handle_execute stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, 0 ->  `Ok stmt
    | 0, _ -> `Error (Error.create stmt)
    | s, _ -> `Wait (Status.of_int s)

  let execute_start stmt () =
    handle_execute stmt B.mysql_stmt_execute_start

  let execute_cont stmt status =
    handle_execute stmt ((flip B.mysql_stmt_execute_cont) status)

  let execute stmt params =
    let n = B.mysql_stmt_param_count stmt.Common.Stmt.raw in
    if n <> Array.length params then
      `Error (Error.make 0 "parameter count mismatch")
    else begin
      match Common.Stmt.bind_params stmt params with
      | `Ok bound -> `Ok (execute_start bound, execute_cont bound)
      | `Error _ as err -> err
    end

  let handle_store_result stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, 0 -> Common.Stmt.bind_result stmt
    | 0, _ -> `Error (Error.create stmt)
    | s, _ -> `Wait (Status.of_int s)

  let store_result_start stmt () =
    handle_store_result stmt B.mysql_stmt_store_result_start

  let store_result_cont stmt status =
    handle_store_result stmt ((flip B.mysql_stmt_store_result_cont) status)

  let store_result stmt =
    (store_result_start stmt, store_result_cont stmt)

  let handle_char stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, '\000' -> `Ok ()
    | 0, _ -> `Error (Error.create stmt)
    | s, _ -> `Wait (Status.of_int s)

  let close_start stmt () =
    handle_char stmt (fun s -> B.mysql_stmt_close_start s)

  let close_cont stmt status =
    handle_char stmt (fun s -> B.mysql_stmt_close_cont s status)

  let close stmt =
    (close_start stmt, close_cont stmt)

  let reset_start stmt () =
    handle_char stmt B.mysql_stmt_reset_start

  let reset_cont stmt status =
    handle_char stmt ((flip B.mysql_stmt_reset_cont) status)

  let reset stmt =
    (reset_start stmt, reset_cont stmt)

  let free_result_start stmt =
    handle_char stmt B.mysql_stmt_free_result_start

  let free_result_cont stmt status =
    handle_char stmt ((flip B.mysql_stmt_free_result_cont) status)

  let handle_next stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, 0 -> `Ok true
    | 0, -1 -> `Ok false
    | 0, _ -> `Error (Error.create stmt)
    | s, _ -> `Wait (Status.of_int s)

  let next_result_start stmt =
    handle_next stmt (B.mysql_stmt_next_result_start)

  let next_result_cont stmt status =
    handle_next stmt ((flip B.mysql_stmt_next_result_cont) status)
end

module Tx = struct
  let handle_tx mariadb f =
    match f mariadb with
    | 0, '\000' -> `Ok mariadb
    | 0, _ -> `Error (Error.create mariadb)
    | s, _ -> `Wait (Status.of_int s)

  let commit_start mariadb () =
    handle_tx mariadb B.mysql_commit_start

  let commit_cont mariadb status =
    handle_tx mariadb ((flip B.mysql_commit_cont) status)

  let commit mariadb =
    (commit_start mariadb, commit_cont mariadb)

  let rollback_start mariadb () =
    handle_tx mariadb B.mysql_rollback_start

  let rollback_cont mariadb status =
    handle_tx mariadb ((flip B.mysql_rollback_cont) status)

  let rollback mariadb =
    (rollback_start mariadb, rollback_cont mariadb)

  let autocommit_start mariadb auto () =
    handle_tx mariadb ((flip B.mysql_autocommit_start) auto)

  let autocommit_cont mariadb status =
    handle_tx mariadb ((flip B.mysql_autocommit_cont) status)

  let autocommit mariadb auto =
    (autocommit_start mariadb auto, autocommit_cont mariadb)
end

module type Wait = sig
  val wait : [< `Connected | `Tx] t -> Status.t -> Status.t
end

module Make (W : Wait) : Mariadb_intf.S = struct
  module Error = Error

  type state = [`Initialized | `Connected | `Tx]
  type 's t = 's mariadb
  type 'a result = ('a, Error.t) Pervasives.result

  type flag
  type server_option = Common.server_option =
    | Multi_statements of bool

  let rec nonblocking m (f, g) =
    match f () with
    | `Ok v -> Ok v
    | `Wait s -> nonblocking m ((fun () -> g (W.wait m s)), g)
    | `Error e -> Error e

  let rec nonblocking_noerr m (f, g) =
    match f () with
    | `Ok -> ()
    | `Wait s -> nonblocking_noerr m ((fun () -> g (W.wait m s)), g)

  module Res = struct
    type 'm t = Res.t
      constraint 'm = [< Mariadb_intf.mode]

    type time = Common.Res.time =
      { year : int
      ; month : int
      ; day : int
      ; hour : int
      ; minute : int
      ; second : int
      }

    type value =
      [ `Int of int
      | `Float of float
      | `String of string
      | `Bytes of bytes
      | `Time of time
      | `Null
      ]

    let fetch res =
      nonblocking res.Common.Res.mariadb (Res.fetch res)

    let num_rows =
      Res.num_rows

    let free res =
      nonblocking_noerr res.Common.Res.mariadb (Res.free res)
  end

  module Stmt = struct
    module Error = struct
      type t = int * string
    end

    type state = [`Prepared | `Bound | `Executed | `Stored | `Fetch]
    type 's t = 's Stmt.t
    type 'a result = ('a, Error.t) Pervasives.result

    type param =
      [ `Tiny of int
      | `Short of int
      | `Int of int
      | `Float of float
      | `Double of float
      | `String of string
      | `Blob of bytes
      ]

    let store_result stmt =
      nonblocking stmt.Common.Stmt.mariadb (Stmt.store_result stmt)

    let handle_execute = function
      | Ok stmt -> store_result stmt
      | Error _ as e -> e

    let execute stmt ps =
      match Stmt.execute stmt ps with
      | `Ok nb -> nonblocking stmt.Common.Stmt.mariadb nb |> handle_execute
      | `Error e ->
          Error e

    let close stmt =
      nonblocking stmt.Common.Stmt.mariadb (Stmt.close stmt)

    let reset stmt =
      nonblocking stmt.Common.Stmt.mariadb (Stmt.reset stmt)
  end

  module Tx = struct
    let commit m = nonblocking m (Tx.commit m)
    let rollback m = nonblocking m (Tx.rollback m)
    let autocommit m b = nonblocking m (Tx.autocommit m b)
  end

  let init = init

  let connect ?host ?user ?pass ?db ?(port=0) ?socket ?(flags=[]) () =
    match init () with
    | Some m ->
        nonblocking m (connect m ?host ?user ?pass ?db ~port ?socket ~flags ())
    | None ->
        Error (2008, "out of memory")

  let close m = nonblocking_noerr m (close m)

  let set_charset m c = nonblocking m (set_charset m c)

  let select_db m db = nonblocking m (select_db m db)

  let change_user m user pass db = nonblocking m (change_user m user pass db)

  let dump_debug_info m = nonblocking m (dump_debug_info m)

  let set_server_option m opt = nonblocking m (set_server_option m opt)

  let ping m = nonblocking m (ping m)

  let prepare m q =
    match prepare m q with
    | `Ok nb -> nonblocking m nb
    | `Error e -> Error e
end
