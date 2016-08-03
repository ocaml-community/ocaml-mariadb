module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

type mode = [`Blocking | `Nonblocking]
type state = [`Unconnected | `Connected | `Tx]

type ('m, 's) t = B.Types.mysql
  constraint 'm = [< mode]
  constraint 's = [< state]

type ('m, 's) mariadb = ('m, 's) t

type flag

type server_option =
  | Multi_statements of bool

type error = int * string

let error mariadb =
  (B.mysql_errno mariadb, B.mysql_error mariadb)

module Bind = struct
  open Ctypes

  type t =
    { n : int
    ; bind : T.Bind.t ptr
    ; length : Unsigned.ulong ptr
    ; is_null : char ptr
    ; is_unsigned : char
    ; error : char ptr
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

  let alloc count =
    { n = count
    ; bind = allocate_n T.Bind.t ~count
    ; length = allocate_n ulong ~count
    ; is_null = allocate_n char ~count
    ; is_unsigned = f
    ; error = allocate_n char ~count
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
  open Ctypes

  type u =
    { mariadb : B.Types.mysql
    ; stmt    : B.Types.stmt
    ; result  : Bind.t
    ; raw     : B.Types.res
    }
  type 'm t = u constraint 'm = [< mode]

  type time =
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

  let create mariadb stmt result raw =
    { mariadb; stmt; result; raw }

  let num_rows res =
    B.mysql_stmt_num_rows res.stmt

  let affected_rows res =
    B.mysql_stmt_affected_rows res.stmt

  let fetch_field res i =
    let open Ctypes in
    coerce (ptr void) (ptr T.Field.t) (B.mysql_fetch_field_direct res.raw i)

  let bytes_of_char_ptr p len =
    let b = Bytes.make len '?' in
    for i = 0 to len - 1 do
      let c = !@(p +@ i) in
      if c <> '\000' then
        Bytes.set b i c;
    done;
    Bytes.copy b

  let get_buffer r at =
    let bp = r.Bind.bind +@ at in
    getf (!@bp) T.Bind.buffer

  let cast buf typ =
    coerce (ptr void) (ptr typ) buf

  let cast_buf r at typ =
    !@(cast (get_buffer r at) typ)

  let to_bytes r at =
    let buf = get_buffer r at in
    let lp = r.Bind.length +@ at in
    let len = Unsigned.ULong.to_int !@lp in
    bytes_of_char_ptr (cast buf char) len

  let to_time r at =
    let buf = get_buffer r at in
    let tp = cast buf T.Time.t in
    let field f = Unsigned.UInt.to_int @@ getf (!@tp) f in
    { year   = field T.Time.year
    ; month  = field T.Time.month
    ; day    = field T.Time.day
    ; hour   = field T.Time.hour
    ; minute = field T.Time.minute
    ; second = field T.Time.second
    }

  let convert r at = function
    | `Null ->
        `Null
    | `Tiny | `Year ->
        `Int (int_of_char @@ cast_buf r at char)
    | `Short ->
        `Int (cast_buf r at int)
    | `Int24 | `Long ->
        `Int (Signed.Int32.to_int @@ cast_buf r at int32_t)
    | `Long_long ->
        `Int (Signed.Int64.to_int @@ cast_buf r at int64_t)
    | `Float ->
        `Float (cast_buf r at float)
    | `Double ->
        `Float (cast_buf r at double)
    | `Decimal | `New_decimal | `String | `Var_string | `Bit ->
        `String (Bytes.to_string (to_bytes r at))
    | `Tiny_blob | `Blob | `Medium_blob | `Long_blob ->
        `Bytes (to_bytes r at)
    | `Time  | `Date | `Datetime | `Timestamp -> `Time (to_time r at)

  let convert_unsigned r at = function
    | `Null -> `Null
    | `Tiny | `Year -> `Int (int_of_char @@ cast_buf r at char)
    | `Short -> `Int (Unsigned.UInt.to_int @@ cast_buf r at uint)
    | `Int24 | `Long -> `Int (Unsigned.UInt32.to_int @@ cast_buf r at uint32_t)
    | `Long_long -> `Int (Unsigned.UInt64.to_int @@ cast_buf r at uint64_t)
    | `Timestamp -> `Time (to_time r at)
    | _ -> failwith "unexpected unsigned type"

  let is_null r at =
    let np = r.Bind.is_null +@ at in
    !@np = '\001'

  let is_unsigned bp =
    getf (!@bp) T.Bind.is_unsigned = '\001'

  let build_row res =
    let r = res.result in
    let n = r.Bind.n in
    Array.init n
      (fun i ->
        let bp = r.Bind.bind +@ i in
        if is_null r i then
          `Null
        else begin
          let typ = getf (!@bp) T.Bind.buffer_type in
          let typ = Bind.buffer_type_of_int typ in
          let conv = if is_unsigned bp then convert_unsigned else convert in
          conv r i typ
        end)
end

let stmt_init mariadb =
  match B.mysql_stmt_init mariadb with
  | Some stmt ->
      B.mysql_stmt_attr_set_bool stmt T.Stmt_attr.update_max_length true;
      Some stmt
  | None ->
      None

module Stmt = struct
  type state = [`Prepared | `Executed]

  type u =
    { raw : B.Types.stmt
    ; mariadb : B.Types.mysql
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

  let error stmt =
    (B.mysql_stmt_errno stmt.raw, B.mysql_error stmt.raw)

  let fetch_field res i =
    let open Ctypes in
    coerce (ptr void) (ptr T.Field.t) (B.mysql_fetch_field_direct res i)

  let alloc_result res =
    let n = B.mysql_num_fields res in
    let r = Bind.alloc n in
    let open Ctypes in
    for i = 0 to n - 1 do
      let bp = r.Bind.bind +@ i in
      let fp = fetch_field res i in
      let flags = getf (!@fp) T.Field.flags in
      let is_unsigned =
        let logand = Unsigned.UInt.logand in
        let unsigned_flag = Unsigned.UInt.of_int T.Field_flags.unsigned in
        if logand flags unsigned_flag <> Unsigned.UInt.zero
        then '\001'
        else '\000' in
      setf (!@bp) T.Bind.buffer_type (getf (!@fp) T.Field.type_);
      setf (!@bp) T.Bind.length (r.Bind.length +@ i);
      setf (!@bp) T.Bind.is_null (r.Bind.is_null +@ i);
      setf (!@bp) T.Bind.is_unsigned is_unsigned;
      setf (!@bp) T.Bind.error (r.Bind.error +@ i)
    done;
    r

  let init mariadb raw =
    let n = B.mysql_stmt_param_count raw in
    match B.mysql_stmt_result_metadata raw with
    | Some res ->
        Some
          { raw
          ; mariadb
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
          `Error (error stmt)

  let malloc count =
    let open Ctypes in
    let p = allocate_n char ~count in
    coerce (ptr char) (ptr void) p

  (* From http://dev.mysql.com/doc/refman/5.7/en/mysql-stmt-fetch.html *)
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
    | `Time | `Date | `Datetime | `Timestamp -> Ctypes.sizeof T.Time.t

  let alloc_size fp typ =
    let open Ctypes in
    match buffer_size typ with
    | -1 -> Unsigned.ULong.to_int (getf (!@fp) T.Field.max_length)
    | n -> n

  let alloc_buffer bp fp =
    let open Ctypes in
    let typ = getf (!@bp) T.Bind.buffer_type in
    let size = alloc_size fp typ in
    setf (!@bp) T.Bind.buffer_length (Unsigned.ULong.of_int size);
    setf (!@bp) T.Bind.buffer (malloc size)

  let bind_result stmt =
    let b = stmt.result in
    let n = b.Bind.n in
    let open Ctypes in
    for i = 0 to n - 1 do
      let bp = b.Bind.bind +@ i in
      let fp = fetch_field stmt.res i in
      alloc_buffer bp fp
    done;
    if B.mysql_stmt_bind_result stmt.raw stmt.result.Bind.bind then
      `Ok (Res.create stmt.mariadb stmt.raw stmt.result stmt.res)
    else
      `Error (error stmt)
end
