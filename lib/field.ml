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
  | `Null
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

let is_null field =
  !@(field.result.Bind.is_null +@ field.at) = '\001'

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

let convert field = function
  | `Null ->
      `Null
  | `Tiny | `Year ->
      `Int (int_of_char @@ cast_to char field)
  | `Short ->
      `Int (cast_to int field)
  | `Int24 | `Long ->
      `Int (Signed.Int32.to_int @@ cast_to int32_t field)
  | `Long_long ->
      `Int (Signed.Int64.to_int @@ cast_to int64_t field)
  | `Float ->
      `Float (cast_to float field)
  | `Double ->
      `Float (cast_to double field)
  | `Decimal | `New_decimal | `String | `Var_string | `Bit ->
      `String (Bytes.to_string @@ to_bytes field)
  | `Tiny_blob | `Blob | `Medium_blob | `Long_blob ->
      `Bytes (to_bytes field)
  | `Time  | `Date | `Datetime | `Timestamp -> `Time (to_time field)

let convert_unsigned field = function
  | `Null -> `Null
  | `Tiny | `Year -> `Int (int_of_char @@ cast_to char field)
  | `Short -> `Int (Unsigned.UInt.to_int @@ cast_to uint field)
  | `Int24 | `Long -> `Int (Unsigned.UInt32.to_int @@ cast_to uint32_t field)
  | `Long_long -> `Int (Unsigned.UInt64.to_int @@ cast_to uint64_t field)
  | `Timestamp -> `Time (to_time field)
  | _ -> failwith "unexpected unsigned type"

let value field =
  if is_null field then
    `Null
  else
    let bp = field.result.Bind.bind +@ field.at in
    let typ = Bind.buffer_type_of_int @@ getf (!@bp) T.Bind.buffer_type in
    let conv = if is_unsigned field then convert_unsigned else convert in
    conv field typ

let int field =
  match value field with
  | `Int i -> i
  | _ -> failwith @@ "field " ^ name field ^ " is not an integer"

let float field =
  match value field with
  | `Float x -> x
  | _ -> failwith @@ "field " ^ name field ^ " is not a float"

let string field =
  match value field with
  | `String s -> s
  | _ -> failwith @@ "field " ^ name field ^ " is not a string"

let bytes field =
  match value field with
  | `Bytes b -> b
  | _ -> failwith @@ "field " ^ name field ^ " is not a byte string"

let time field =
  match value field with
  | `Time t -> t
  | _ -> failwith @@ "field " ^ name field ^ " is not a time value"
