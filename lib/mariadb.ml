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


let flip f = fun x y -> f y x

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

  type buffer_type =
    [ `Null
    | `Tiny
    | `Year
    | `Short
    | `Int24
    | `Long
    | `Float
    | `Long_long
    | `Double
    | `Decimal
    | `New_decimal
    | `String
    | `Var_string
    | `Tiny_blob
    | `Blob
    | `Medium_blob
    | `Long_blob
    | `Bit
    | `Time
    | `Date
    | `Datetime
    | `Timestamp
    ]

  let buffer_type_of_int i =
    let open T.Type in
    if i = null              then `Null
    else if i = tiny         then `Tiny
    else if i = year         then `Year
    else if i = short        then `Short
    else if i = int24        then `Int24
    else if i = long         then `Long
    else if i = float        then `Float
    else if i = long_long    then `Long_long
    else if i = double       then `Double
    else if i = decimal      then `Decimal
    else if i = new_decimal  then `New_decimal
    else if i = string       then `String
    else if i = var_string   then `Var_string
    else if i = tiny_blob    then `Tiny_blob
    else if i = blob         then `Blob
    else if i = medium_blob  then `Medium_blob
    else if i = long_blob    then `Long_blob
    else if i = bit          then `Bit
    else if i = time         then `Time
    else if i = date         then `Date
    else if i = datetime     then `Datetime
    else if i = timestamp    then `Timestamp
    else invalid_arg @@ "unknown buffer type " ^ (string_of_int i)

  let t = '\001'
  let f = '\000'

  let alloc n =
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
  match B.mysql_stmt_init mariadb with
  | Some stmt ->
      B.mysql_stmt_attr_set_bool stmt T.Stmt_attr.update_max_length true;
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
    match Bind.buffer_type_of_int typ with
    | `Null -> 0
    | `Tiny | `Year -> 1
    | `Short -> 2
    | `Int24 | `Long | `Float -> 4
    | `Long_long | `Double -> 8
    | `Decimal | `New_decimal | `String | `Var_string
    | `Tiny_blob | `Blob | `Medium_blob | `Long_blob | `Bit -> -1
    | `Time | `Date | `Datetime | `Timestamp -> assert false (* TODO *)

  let alloc_buffer bp fp typ =
    let open Ctypes in
    let to_ulong = Unsigned.ULong.of_int in
    let of_ulong = Unsigned.ULong.to_int in
    let size =
      match buffer_size typ with
      | -1 -> of_ulong (getf (!@fp) T.Field.max_length)
      | n -> n in
    setf (!@bp) T.Bind.buffer_length (to_ulong size);
    setf (!@bp) T.Bind.buffer (malloc size)

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
      | Multi_statements true -> T.Server_options.multi_statements_on
      | Multi_statements false -> T.Server_options.multi_statements_off
    in
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
    match Stmt.init raw with
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
    match stmt_init mariadb with
    | Some raw ->
        `Ok (prepare_start mariadb raw query, prepare_cont mariadb raw)
    | None ->
        `Error (Error.create mariadb)

  module Res = struct
    type t = [`Nonblocking] Res.t
    type value =
      [ `Int of int
      | `Float of float
      | `String of string
      | `Bytes of bytes
      | `Null
      ]

    let handle_fetch_row res f =
      match f res.Res.raw with
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
      let bp = r.Bind.bind +@ at in
      let buf = getf (!@bp) T.Bind.buffer in
      let cast_to t =
        !@(coerce (ptr void) (ptr t) buf) in
      let to_char_buffer () =
        let lp = r.Bind.length +@ at in
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
          assert false (* TODO *)

    let build_row res =
      let r = res.Res.result in
      let n = r.Bind.n in
      let open Ctypes in
      Array.init n
        (fun i ->
          let bp = r.Bind.bind +@ i in
          (* TODO test is_null *)
          let buffer_type = getf (!@bp) T.Bind.buffer_type in
          (* TODO test is_unsigned *)
          convert r i (Bind.buffer_type_of_int buffer_type))

    let handle_fetch res f =
      match f res.Res.stmt with
      | 0, 0 -> `Ok (Some (build_row res))
      | 0, 1 ->
          let errno = B.mysql_stmt_errno res.Res.stmt in
          let error = B.mysql_stmt_error res.Res.stmt in
          `Error (Stmt.Error.make errno error)
      | 0, r when r = T.Return_code.no_data -> `Ok None
      | 0, r when r = T.Return_code.data_truncated ->
          `Error (Stmt.Error.make 0 "truncated data")
      | s, _ -> `Wait (Status.of_int s)

    let fetch_start res () =
      handle_fetch res B.mysql_stmt_fetch_start

    let fetch_cont stmt status =
      handle_fetch stmt ((flip B.mysql_stmt_fetch_cont) status)

    let fetch res =
      (fetch_start res, fetch_cont res)

    let handle_ok_wait res f =
      match f res.Res.raw with
      | 0 -> `Ok
      | s -> `Wait (Status.of_int s)

    let free_start res () =
      handle_ok_wait res B.mysql_free_result_start

    let free_cont res status =
      handle_ok_wait res ((flip B.mysql_free_result_cont) status)

    let free res =
      (free_start res, free_cont res)
  end

  module Stmt = struct
    type 's t = ([`Nonblocking], 's) Stmt.t
      constraint 's = [< Stmt.state]

    type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Stmt.Error.t]

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

    let handle_char stmt f =
      match f stmt.Stmt.raw with
      | 0, '\000' -> `Ok ()
      | 0, _ -> `Error (Stmt.Error.create stmt)
      | s, _ -> `Wait (Status.of_int s)

    let close_start stmt () =
      handle_char stmt (fun s -> B.mysql_stmt_close_start s)

    let close_cont stmt status =
      handle_char stmt (fun s -> B.mysql_stmt_close_cont s status)

    let close stmt =
      (close_start stmt, close_cont stmt)

    let reset_start stmt () =
      handle_char stmt (fun s -> B.mysql_stmt_reset_start s)

    let reset_cont stmt status =
      handle_char stmt (fun s -> B.mysql_stmt_reset_cont s status)

    let reset stmt =
      (reset_start stmt, reset_cont stmt)

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
end

let init () =
  B.mysql_init ()

let close =
  B.mysql_close

let use_result =
  B.mysql_use_result
