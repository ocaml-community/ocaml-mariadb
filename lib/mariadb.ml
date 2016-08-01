module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

type mode = [`Blocking | `Nonblocking]
type state = [`Initialized | `Connected | `Tx]

type ('m, 's) t = B.Types.mysql
  constraint 'm = [< mode]
  constraint 's = [< state]

type ('m, 's) mariadb = ('m, 's) t

type row = string array

type flag

type server_option =
  | Multi_statements of bool

module Error = struct
  type t = int * string

  let create mariadb =
    (B.mysql_errno mariadb, B.mysql_error mariadb)

  let errno = fst
  let message = snd
  let make errno msg = (errno, msg)
end

module Bind = struct
  open Ctypes

  type t =
    { n : int
    ; bind : T.Bind.t ptr
    ; length : Unsigned.ulong ptr
    ; is_null : char ptr
    ; is_unsigned : char
    }

  let t = '\001'
  let f = '\000'

  let alloc n =
    Printf.printf "allocatng %d binds - %d each\n%!" n (sizeof T.Bind.t);
    { n
    ; bind = allocate_n T.Bind.t ~count:n
    ; length = allocate_n ulong ~count:n
    ; is_null = allocate_n char ~count:n
    ; is_unsigned = f
    }

  let bind b ~buffer ~size ~mysql_type ~unsigned ~at =
    assert (at >= 0 && at < b.n);
    let size = Unsigned.ULong.of_int size in
    let bp = b.bind +@ at in
    let lp = b.length +@ at in
    lp <-@ size;
    setf (!@bp) T.Bind.length lp;
    setf (!@bp) T.Bind.is_unsigned unsigned;
    setf (!@bp) T.Bind.buffer_type mysql_type;
    setf (!@bp) T.Bind.buffer_length size;
    setf (!@bp) T.Bind.buffer buffer

  let tiny ?(unsigned = false) b param ~at =
    let p = allocate char (char_of_int param) in
    bind b
      ~buffer:(coerce (ptr char) (ptr void) p)
      ~size:(sizeof int)
      ~mysql_type:T.Type.tiny
      ~unsigned:(if unsigned then t else f)
      ~at

  let short ?(unsigned = false) b param ~at =
    let p = allocate short param in
    bind b
      ~buffer:(coerce (ptr short) (ptr void) p)
      ~size:(sizeof int)
      ~mysql_type:T.Type.short
      ~unsigned:(if unsigned then t else f)
      ~at

  let int ?(unsigned = false) b param ~at =
    let p = allocate int param in
    bind b
      ~buffer:(coerce (ptr int) (ptr void) p)
      ~size:(sizeof int)
      ~mysql_type:T.Type.long_long
      ~unsigned:(if unsigned then t else f)
      ~at

  let float b param ~at =
    let p = allocate float param in
    bind b
      ~buffer:(coerce (ptr float) (ptr void) p)
      ~size:(sizeof float)
      ~mysql_type:T.Type.float
      ~unsigned:f
      ~at

  let double b param ~at =
    let p = allocate double param in
    bind b
      ~buffer:(coerce (ptr double) (ptr void) p)
      ~size:(sizeof double)
      ~mysql_type:T.Type.double
      ~unsigned:f
      ~at

  let string b param ~at =
    let len = String.length param in
    let p = allocate_n char ~count:len in
    String.iteri (fun i c -> (p +@ i) <-@ c) param;
    bind b
      ~buffer:(coerce (ptr char) (ptr void) p)
      ~size:len
      ~mysql_type:T.Type.string
      ~unsigned:f
      ~at

  let blob b param ~at =
    let len = Bytes.length param in
    let p = allocate_n char ~count:len in
    String.iteri (fun i c -> (p +@ i) <-@ c) param;
    bind b
      ~buffer:(coerce (ptr char) (ptr void) p)
      ~size:len
      ~mysql_type:T.Type.blob
      ~unsigned:f
      ~at
end

module Res = struct
  type u =
    { stmt   : B.Types.stmt
    ; result : Bind.t
    ; raw    : B.Types.res
    }
  type 'm t = u constraint 'm = [< mode]

  let create stmt result raw =
    { stmt; result; raw }

  let num_rows res =
    B.mysql_stmt_num_rows res.stmt

  let fetch_field res i =
    let open Ctypes in
    coerce (ptr void) (ptr T.Field.t) (B.mysql_fetch_field_direct res.raw i)

  let free res =
    B.mysql_free_result res.raw
end

let stmt_init mariadb =
  let attr = T.Stmt_attr.update_max_length in
  match B.mysql_stmt_init mariadb with
  | Some stmt ->
      B.mysql_stmt_attr_set_bool stmt attr true;
      Some stmt
  | None ->
      None

module Stmt = struct
  type state = [`Prepared | `Bound | `Executed | `Stored | `Fetch]

  type u =
    { raw : B.Types.stmt
    ; res : B.Types.res
    ; num_params : int
    ; params : Bind.t
    ; result : Bind.t
    }
  type ('m, 's) t = u
    constraint 'm = [< mode]
    constraint 's = [< state]

  type cursor_type
    = No_cursor
    | Read_only

  type attr
    = Update_max_length of bool
    | Cursor_type of cursor_type
    | Prefetch_rows of int

  type param =
    [ `Tiny of int
    | `Short of int
    | `Int of int
    | `Float of float
    | `Double of float
    | `String of string
    | `Blob of bytes
    ]

  module Error = struct
    type ('m, 's) stmt = ('m, 's) t
    type t = int * string

    let create stmt =
      (B.mysql_stmt_errno stmt.raw, B.mysql_error stmt.raw)

    let errno = fst
    let message = snd

    let make errno msg = (errno, msg)
  end

  type 'a result = [`Ok of 'a | `Error of Error.t]

  let fetch_field res i =
    let open Ctypes in
    coerce (ptr void) (ptr T.Field.t) (B.mysql_fetch_field_direct res i)

  let alloc_result res =
    let n = B.mysql_num_fields res in
    let r = Bind.alloc n in
    for i = 0 to n - 1 do
      let open Ctypes in
      let bp = r.Bind.bind +@ i in
      let fp = fetch_field res i in
      let flags = Unsigned.UInt.to_int @@ getf (!@fp) T.Field.flags in
      let is_unsigned =
        if flags land T.Field_flags.unsigned <> 0 then '\001'
        else '\001' in
      setf (!@bp) T.Bind.buffer_type (getf (!@fp) T.Field.type_);
      setf (!@bp) T.Bind.length (r.Bind.length +@ i);
      setf (!@bp) T.Bind.is_null (r.Bind.is_null +@ i);
      setf (!@bp) T.Bind.is_unsigned is_unsigned
    done;
    r

  let init raw =
    let n = B.mysql_stmt_param_count raw in
    match B.mysql_stmt_result_metadata raw with
    | Some res ->
        Some
          { raw
          ; res
          ; num_params = n
          ; params = Bind.alloc n
          ; result = alloc_result res
          }
    | None ->
        None

  let bind_params stmt params =
    match Array.length params with
    | 0 -> `Ok stmt
    | n ->
        let b = Bind.alloc n in
        Array.iteri
          (fun at arg ->
            match arg with
            | `Tiny i -> Bind.tiny b i ~at
            | `Short i -> Bind.short b i ~at
            | `Int i -> Bind.int b i ~at
            | `Float x -> Bind.float b x ~at
            | `Double x -> Bind.double b x ~at
            | `String s -> Bind.string b s ~at
            | `Blob s -> Bind.blob b s ~at)
          params;
        if B.mysql_stmt_bind_param stmt.raw b.Bind.bind then
          `Ok stmt
        else
          `Error (Error.create stmt)

  let malloc n =
    let open Ctypes in
    let p = allocate_n char ~count:n in
    coerce (ptr char) (ptr void) p

  let buffer_size typ =
    let open T.Type in
    if typ == null then Some 0
    else if typ == tiny || typ == year then Some 1
    else if typ == short then Some 2
    else if typ == int24 || typ == long || typ == float then Some 4
    else if typ == long_long || typ == double then Some 8
    else if typ == decimal || typ = new_decimal || typ == string
         || typ == var_string || typ == bit || typ == tiny_blob
         || typ == blob || typ == medium_blob || typ == long_blob
      then Some (-1)
(* TODO else if typ == time || typ == date || typ == datetime || typ == timestamp
      then Some (sizeof T.Time.t) *)
    else
      None

  let alloc_buffer bp fp typ =
    let open Ctypes in
    let to_ulong = Unsigned.ULong.of_int in
    let of_ulong = Unsigned.ULong.to_int in
    let size =
      match buffer_size typ with
      | Some (-1) -> of_ulong (getf (!@fp) T.Field.max_length)
      | Some n -> n
      | None -> -1 in
    if size > 0 then begin
      setf (!@bp) T.Bind.buffer_length (to_ulong size);
      setf (!@bp) T.Bind.buffer (malloc size)
    end

  let bind_result stmt =
    let n = stmt.result.Bind.n in
    let open Ctypes in
    for i = 0 to n - 1 do
      let bp = stmt.result.Bind.bind +@ i in
      let fp = fetch_field stmt.res i in
      let typ = getf (!@fp) T.Field.type_ in
      alloc_buffer bp fp typ
    done;
    if B.mysql_stmt_bind_result stmt.raw stmt.result.Bind.bind then
      `Ok (Res.create stmt.raw stmt.result stmt.res)
    else
      `Error (Error.create stmt)
end

module Nonblocking = struct
  module Status = Wait_status

  type 's t = ([`Nonblocking], 's) mariadb
  type 's mariadb = 's t
  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Error.t]

  type 'a startfun = unit -> 'a result
  type 'a contfun = Status.t -> 'a result
  type 'a nonblocking = 'a startfun * 'a contfun

  type 'a fetch_result = ['a result | `Done]

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

  let handle_conn mariadb f =
    match handle_opt mariadb f with
    | `Ok raw -> `Ok mariadb
    | `Wait s -> `Wait s
    | `Error e -> `Error e

  let connect_start mariadb ?host ?user ?pass ?db ?(port = 0) ?socket
                    ?(flags = []) () =
    (* TODO flags *)
    handle_conn mariadb
      (fun m -> B.mysql_real_connect_start m host user pass db port socket 0)

  let connect_cont mariadb status =
    handle_conn mariadb
      (fun m -> B.mysql_real_connect_cont m (Status.to_int status))

  let query_start mariadb query =
    handle_int mariadb (fun m -> B.mysql_real_query_start m query)

  let query_cont mariadb status =
    handle_int mariadb (fun m -> B.mysql_real_query_cont m status)

  let handle_ok_wait mariadb f =
    match f mariadb with
    | 0 -> `Ok
    | s -> `Wait (Status.of_int s)

  let close_start mariadb =
    handle_ok_wait mariadb (fun raw -> B.mysql_close_start raw)

  let close_cont mariadb status =
    handle_ok_wait mariadb (fun raw -> B.mysql_close_cont raw status)

  let fd =
    B.mysql_get_socket

  let timeout =
    B.mysql_get_timeout_value

  let set_charset_start mariadb charset =
    handle_unit mariadb (fun m -> B.mysql_set_character_set_start m charset)

  let set_charset_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_set_character_set_cont m status)

  let select_db_start mariadb db =
    handle_unit mariadb (fun m -> B.mysql_select_db_start m db)

  let select_db_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_select_db_cont m status)

  let change_user_start mariadb user pass db =
    handle_unit mariadb (fun m -> B.mysql_change_user_start m user pass db)

  let change_user_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_change_user_cont m status)

  let dump_debug_info_start mariadb =
    handle_unit mariadb (fun m -> B.mysql_dump_debug_info_start m)

  let dump_debug_info_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_dump_debug_info_cont m status)

  let set_server_option_start mariadb opt =
    let opt =
      match opt with
      | Multi_statements true -> T.Server_options.multi_statements_on
      | Multi_statements false -> T.Server_options.multi_statements_off
    in
    handle_unit mariadb (fun m -> B.mysql_set_server_option_start m opt)

  let set_server_option_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_set_server_option_cont m status)

  let ping_start mariadb =
    handle_unit mariadb (fun m -> B.mysql_ping_start m)

  let ping_cont mariadb status =
    handle_unit mariadb (fun m -> B.mysql_ping_cont m status)

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
    match Stmt.init raw with
    | Some stmt -> `Ok stmt
    | None -> `Error (Error.create mariadb)

  let handle_prepare mariadb raw f =
    match f raw with
    | 0, 0 -> build_stmt mariadb raw
    | 0, _ -> `Error (B.mysql_stmt_errno raw, B.mysql_stmt_error raw)
    | s, _ -> `Wait (Status.of_int s)

  let flip f = fun x y -> f y x

  let prepare_start mariadb raw_stmt query () =
    handle_prepare mariadb raw_stmt ((flip B.mysql_stmt_prepare_start) query)

  let prepare_cont mariadb raw_stmt status =
    handle_prepare mariadb raw_stmt ((flip B.mysql_stmt_prepare_cont) status)

  let prepare mariadb query =
    match stmt_init mariadb with
    | Some raw ->
        `Ok (prepare_start mariadb raw query, prepare_cont mariadb raw)
    | None ->
        `Error (Error.create mariadb)

  module Res = struct
    type t = [`Nonblocking] Res.t

    let handle_fetch_row res f =
      match f res.Res.raw with
      | 0, Some row -> `Ok row
      | 0, None -> `Done
      | s, _ -> `Wait (Status.of_int s)

    let fetch_row_start res =
      handle_fetch_row res B.mysql_fetch_row_start

    let fetch_row_cont res status =
      handle_fetch_row res ((flip B.mysql_fetch_row_cont) status)

    let handle_ok_wait res f =
      match f res.Res.raw with
      | 0 -> `Ok
      | s -> `Wait (Status.of_int s)

    let free_start res =
      handle_ok_wait res B.mysql_free_result_start

    let free_cont res status =
      handle_ok_wait res ((flip B.mysql_free_result_cont) status)
  end

  module Stmt = struct
    type 's t = ([`Nonblocking], 's) Stmt.t
      constraint 's = [< Stmt.state]

    type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Stmt.Error.t]

    type 'a fetch_result = ['a result | `Done]

    let init =
      Stmt.init

    let handle_execute stmt f =
      match f stmt.Stmt.raw with
      | 0, 0 ->  `Ok stmt
      | 0, _ -> `Error (Stmt.Error.create stmt)
      | s, _ -> `Wait (Status.of_int s)

    let execute_start stmt () =
      handle_execute stmt B.mysql_stmt_execute_start

    let execute_cont stmt status =
      handle_execute stmt ((flip B.mysql_stmt_execute_cont) status)

    let execute (stmt: ([< mode ], [< Stmt.state ]) Stmt.t) params =
      let n = B.mysql_stmt_param_count stmt.Stmt.raw in
      if n <> Array.length params then
        `Error (Error.make 0 "parameter count mismatch")
      else begin
        match Stmt.bind_params stmt params with
        | `Ok bound -> `Ok (execute_start bound, execute_cont bound)
        | `Error _ as err -> err
      end

    let handle_store_result stmt f =
      match f stmt.Stmt.raw with
      | 0, 0 -> Stmt.bind_result stmt
      | 0, _ -> `Error (Stmt.Error.create stmt)
      | s, _ -> `Wait (Status.of_int s)

    let store_result_start stmt () =
      handle_store_result stmt B.mysql_stmt_store_result_start

    let store_result_cont stmt status =
      handle_store_result stmt ((flip B.mysql_stmt_store_result_cont) status)

    let store_result stmt =
      (store_result_start stmt, store_result_cont stmt)

    let handle_fetch stmt f =
      match f stmt.Stmt.raw with
      | 0, 0 -> `Ok stmt
      | 0, 1 -> `Error (Stmt.Error.create stmt)
      | 0, r when r = T.Return_code.no_data -> `Done
      | 0, r when r = T.Return_code.data_truncated ->
          `Error (Stmt.Error.make 0 "truncated data")
      | s, _ -> `Wait (Status.of_int s)

    let fetch_start stmt =
      handle_fetch stmt (fun s -> B.mysql_stmt_fetch_start s)

    let fetch_cont stmt status =
      handle_fetch stmt (fun s -> B.mysql_stmt_fetch_cont s status)

    let handle_char stmt f =
      match f stmt.Stmt.raw with
      | 0, '\000' -> `Ok ()
      | 0, _ -> `Error (Stmt.Error.create stmt)
      | s, _ -> `Wait (Status.of_int s)

    let close_start stmt =
      handle_char stmt (fun s -> B.mysql_stmt_close_start s)

    let close_cont stmt status =
      handle_char stmt (fun s -> B.mysql_stmt_close_cont s status)

    let reset_start stmt =
      handle_char stmt (fun s -> B.mysql_stmt_reset_start s)

    let reset_cont stmt status =
      handle_char stmt (fun s -> B.mysql_stmt_reset_cont s status)

    let free_result_start stmt =
      handle_char stmt (fun s -> B.mysql_stmt_free_result_start s)

    let free_result_cont stmt status =
      handle_char stmt (fun s -> B.mysql_stmt_free_result_cont s status)

    let handle_next stmt f =
      match f stmt.Stmt.raw with
      | 0, 0 -> `Ok true
      | 0, -1 -> `Ok false
      | 0, _ -> `Error (Stmt.Error.create stmt)
      | s, _ -> `Wait (Status.of_int s)

    let next_result_start stmt =
      handle_next stmt (fun s -> B.mysql_stmt_next_result_start s)

    let next_result_cont stmt status =
      handle_next stmt (fun s -> B.mysql_stmt_next_result_cont s status)
  end

  module Tx = struct
    let handle_tx mariadb f =
      match f mariadb with
      | 0, '\000' -> `Ok mariadb
      | 0, _ -> `Error (Error.create mariadb)
      | s, _ -> `Wait (Status.of_int s)

    let commit_start mariadb =
      handle_tx mariadb B.mysql_commit_start

    let commit_cont mariadb status =
      handle_tx mariadb ((flip B.mysql_commit_cont) status)

    let rollback_start mariadb =
      handle_tx mariadb B.mysql_rollback_start

    let rollback_cont mariadb status =
      handle_tx mariadb ((flip B.mysql_rollback_cont) status)

    let autocommit_start mariadb auto =
      handle_tx mariadb ((flip B.mysql_autocommit_start) auto)

    let autocommit_cont mariadb status =
      handle_tx mariadb ((flip B.mysql_autocommit_cont) status)
  end
end

let init () =
  B.mysql_init ()

let close =
  B.mysql_close

let use_result =
  B.mysql_use_result
