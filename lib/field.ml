open Ctypes

module T = Ffi_bindings.Types(Ffi_generated_types)

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
  | `NullInt of int option
  | `NullFloat of float option
  | `NullString of string option
  | `NullBytes of bytes option
  | `NullTime of time option
  ]

type t =
  { result : Bind.t
  ; pointer : T.Field.t ptr
  ; at : int
  }

let create result pointer at =
  { result; pointer; at }

let name field =
  getf (!@(field.pointer)) T.Field.name

let null_value field =
  !@(field.result.Bind.is_null +@ field.at) = '\001'

let can_be_null field =
  let flags = getf (!@(field.pointer)) T.Field.flags in
  Unsigned.UInt.logand flags T.Field.Flags.not_null = Unsigned.UInt.zero

let is_unsigned field =
  let bp = field.result.Bind.bind +@ field.at in
  getf (!@bp) T.Bind.is_unsigned = '\001'

let buffer field =
  let bp = field.result.Bind.bind +@ field.at in
  getf (!@bp) T.Bind.buffer

let cast_to typ field =
  !@(coerce (ptr void) (ptr typ) (buffer field))

let to_bytes field =
  let buf = buffer field in
  let r = field.result in
  let lp = r.Bind.length +@ field.at in
  let len = Unsigned.ULong.to_int !@lp in
  let p = coerce (ptr void) (ptr char) buf in
  Bytes.init len (fun i -> !@(p +@ i))

let to_time field =
  let buf = buffer field in
  let tp = coerce (ptr void) (ptr T.Time.t) buf in
  let member f = Unsigned.UInt.to_int @@ getf (!@tp) f in
  { year   = member T.Time.year
  ; month  = member T.Time.month
  ; day    = member T.Time.day
  ; hour   = member T.Time.hour
  ; minute = member T.Time.minute
  ; second = member T.Time.second
  }

let wrap field v =
  if null_value field then None
  else Some v

let convert field = function
  | `Tiny | `Year ->
      let i = int_of_char (cast_to char field) in
      if can_be_null field then `NullInt (wrap field i) else `Int i

  | `Short ->
      let i = cast_to int field in
      if can_be_null field then `NullInt (wrap field i) else `Int i

  | `Int24 | `Long ->
      let i = Signed.Int32.to_int (cast_to int32_t field) in
      if can_be_null field then `NullInt (wrap field i) else `Int i

  | `Long_long ->
      let i = Signed.Int64.to_int (cast_to int64_t field) in
      if can_be_null field then `NullInt (wrap field i) else `Int i

  | `Float ->
      let x = cast_to float field in
      if can_be_null field then `NullFloat (wrap field x) else `Float x

  | `Double ->
      let x = cast_to double field in
      if can_be_null field then `NullFloat (wrap field x) else `Float x

  | `Decimal | `New_decimal | `String | `Var_string | `Bit ->
      let s = Bytes.to_string (to_bytes field) in
      if can_be_null field then `NullString (wrap field s) else `String s

  | `Tiny_blob | `Blob | `Medium_blob | `Long_blob ->
      let b = to_bytes field in
      if can_be_null field then `NullBytes (wrap field b) else `Bytes b

  | `Time  | `Date | `Datetime | `Timestamp ->
      let t = to_time field in
      if can_be_null field then `NullTime (wrap field t) else `Time t

  | `Null ->
      failwith "field shouldn't have null type"

let convert_unsigned field typ =
  let wrap i =
    if can_be_null field then `NullInt (wrap field i)
    else `Int i in
  wrap @@
    match typ with
    | `Tiny | `Year -> int_of_char (cast_to char field)
    | `Short -> Unsigned.UInt.to_int (cast_to uint field)
    | `Int24 | `Long -> Unsigned.UInt32.to_int (cast_to uint32_t field)
    | `Long_long -> Unsigned.UInt64.to_int (cast_to uint64_t field)
    | _ -> failwith "unexpected unsigned type"

let value field =
  let bp = field.result.Bind.bind +@ field.at in
  let typ = Bind.buffer_type_of_int @@ getf (!@bp) T.Bind.buffer_type in
  let conv = if is_unsigned field then convert_unsigned else convert in
  conv field typ

let err field ~info =
  failwith @@ "field '" ^ name field ^ "' is not " ^ info

let int field =
  match value field with
  | `Int i -> i
  | _ -> err field ~info:"an integer"

let float field =
  match value field with
  | `Float x -> x
  | _ -> err field ~info:"a float"

let string field =
  match value field with
  | `String s -> s
  | _ -> err field ~info:"a string"

let bytes field =
  match value field with
  | `Bytes b -> b
  | _ -> err field ~info:"a byte string"

let time field =
  match value field with
  | `Time t -> t
  | _ -> err field ~info:"a time value"

let null_int field =
  match value field with
  | `NullInt i -> i
  | _ -> err field ~info:"a nullable integer"

let null_float field =
  match value field with
  | `NullFloat x -> x
  | _ -> err field ~info:"a nullable float"

let null_string field =
  match value field with
  | `NullString s -> s
  | _ -> err field ~info:"a nullable string"

let null_bytes field =
  match value field with
  | `NullBytes b -> b
  | _ -> err field ~info:"a nullable byte string"

let null_time field =
  match value field with
  | `NullTime t -> t
  | _ -> err field ~info:"a nullable time value"
