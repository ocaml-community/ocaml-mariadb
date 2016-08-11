module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

module Row = Row
module Field = Field

type mode = [`Blocking | `Nonblocking]
type 'm t = B.Types.mysql constraint 'm = [< mode]

type 'm mariadb = 'm t

type flag =
  | Can_handle_expired_passwords
  | Compress
  | Found_rows
  | Ignore_sigpipe
  | Ignore_space
  | Interactive
  | Local_files
  | Multi_results
  | Multi_statements
  | No_schema
  | ODBC
  | SSL
  | Remember_options

type server_option =
  | Multi_statements of bool

type error = int * string

let error mariadb =
  (B.mysql_errno mariadb, B.mysql_error mariadb)

let int_of_server_option = function
  | Multi_statements true -> T.Server_options.multi_statements_on
  | Multi_statements false -> T.Server_options.multi_statements_off

let int_of_flag = function
  | Can_handle_expired_passwords -> T.Flags.can_handle_expired_passwords
  | Compress -> T.Flags.compress
  | Found_rows -> T.Flags.found_rows
  | Ignore_sigpipe -> T.Flags.ignore_sigpipe
  | Ignore_space -> T.Flags.ignore_space
  | Interactive -> T.Flags.interactive
  | Local_files -> T.Flags.local_files
  | Multi_results -> T.Flags.multi_results
  | Multi_statements -> T.Flags.multi_statements
  | No_schema -> T.Flags.no_schema
  | ODBC -> T.Flags.odbc
  | SSL -> T.Flags.ssl
  | Remember_options -> T.Flags.remember_options

let int_of_flags =
  List.fold_left (fun acc flag -> acc lor int_of_flag flag) 0

module Res = struct
  open Ctypes

  type u =
    { mariadb : B.Types.mysql
    ; stmt    : B.Types.stmt
    ; result  : Bind.t
    ; raw     : B.Types.res
    ; buffers : unit ptr array
    }
  type 'm t = u constraint 'm = [< mode]

  let create ~mariadb ~stmt ~result ~raw ~buffers =
    { mariadb; stmt; result; raw; buffers }

  let num_rows res =
    B.mysql_stmt_num_rows res.stmt

  let affected_rows res =
    B.mysql_stmt_affected_rows res.stmt

  let fetch_field res i =
    coerce (ptr void) (ptr T.Field.t) (B.mysql_fetch_field_direct res.raw i)

  let build_row (type t) (module R : Row.S with type t = t) res =
    let r = res.result in
    R.build r.Bind.n
      (fun i ->
        let fp = fetch_field res i in
        Field.create r fp i)

  let stream (type t) (module R : Row.S with type t = t) res fetch =
    let module M = struct exception E of error end in
    let next _ =
      match fetch (module R : Row.S with type t = t) res with
      | Ok (Some _ as row) -> row
      | Ok None -> None
      | Error e -> raise (M.E e) in
    try Ok (Stream.from next)
    with M.E e -> Error e
end

let stmt_init mariadb =
  match B.mysql_stmt_init mariadb with
  | Some stmt ->
      B.mysql_stmt_attr_set_bool stmt T.Stmt_attr.update_max_length true;
      Some stmt
  | None ->
      None

module Stmt = struct
  open Ctypes

  type u =
    { raw : B.Types.stmt
    ; mariadb : B.Types.mysql
    ; res : B.Types.res
    ; num_params : int
    ; params : Bind.t
    ; result : Bind.t
    ; result_buffers : unit ptr array
    }
  type 'm t = u constraint 'm = [< mode]

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

  let error stmt =
    (B.mysql_stmt_errno stmt.raw, B.mysql_error stmt.raw)

  let fetch_field res i =
    coerce (ptr void) (ptr T.Field.t) (B.mysql_fetch_field_direct res i)

  let test_unsigned flags =
    Unsigned.UInt.logand flags T.Field.Flags.unsigned <> Unsigned.UInt.zero

  let alloc_result res n =
    let r = Bind.alloc n in
    for i = 0 to n - 1 do
      let bp = r.Bind.bind +@ i in
      let fp = fetch_field res i in
      let flags = getf (!@fp) T.Field.flags in
      let is_unsigned = if test_unsigned flags then '\001' else '\000' in
      setf (!@bp) T.Bind.buffer_type (getf (!@fp) T.Field.typ);
      setf (!@bp) T.Bind.length (r.Bind.length +@ i);
      setf (!@bp) T.Bind.is_null (r.Bind.is_null +@ i);
      setf (!@bp) T.Bind.is_unsigned is_unsigned;
      setf (!@bp) T.Bind.error (r.Bind.error +@ i)
    done;
    r

  let init mariadb raw =
    let np = B.mysql_stmt_param_count raw in
    match B.mysql_stmt_result_metadata raw with
    | Some res ->
        let nf = B.mysql_num_fields res in
        Some
          { raw
          ; mariadb
          ; res
          ; num_params = np
          ; params = Bind.alloc np
          ; result = alloc_result res nf
          ; result_buffers = Array.make nf null
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
          `Error (error stmt)

  (* From http://dev.mysql.com/doc/refman/5.7/en/mysql-stmt-fetch.html *)
  let buffer_size typ =
    match Bind.buffer_type_of_int typ with
    | `Null -> 0
    | `Tiny | `Year -> 1
    | `Short -> 2
    | `Int24 | `Long | `Float -> 4
    | `Long_long | `Double -> 8
    | `Decimal | `New_decimal | `String | `Var_string
    | `Tiny_blob | `Blob | `Medium_blob | `Long_blob | `Bit -> -1
    | `Time | `Date | `Datetime | `Timestamp -> Ctypes.sizeof T.Time.t

  let malloc count =
    let p = allocate_n char ~count in
    coerce (ptr char) (ptr void) p

  let alloc_buffer stmt bp fp i =
    let typ = getf (!@bp) T.Bind.buffer_type in
    match buffer_size typ with
    | -1 ->
        let n = getf (!@fp) T.Field.max_length in
        setf (!@bp) T.Bind.buffer_length n;
        stmt.result_buffers.(i) <- malloc (Unsigned.ULong.to_int n);
        setf (!@bp) T.Bind.buffer stmt.result_buffers.(i)
    | n ->
        setf (!@bp) T.Bind.buffer_length (Unsigned.ULong.of_int n);
        stmt.result_buffers.(i) <- malloc n;
        setf (!@bp) T.Bind.buffer stmt.result_buffers.(i)

  let bind_result stmt =
    let b = stmt.result in
    let n = b.Bind.n in
    for i = 0 to n - 1 do
      let bp = b.Bind.bind +@ i in
      let fp = fetch_field stmt.res i in
      alloc_buffer stmt bp fp i
    done;
    if B.mysql_stmt_bind_result stmt.raw stmt.result.Bind.bind then
      let res =
        Res.create
          ~mariadb:stmt.mariadb
          ~stmt:stmt.raw
          ~result:stmt.result
          ~raw:stmt.res
          ~buffers:stmt.result_buffers in
      `Ok res
    else
      `Error (error stmt)
end
