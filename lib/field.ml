open Ctypes

module T = Ffi_generated.Types

type value =
  [ `Null
  | `Int of int
  | `Int64 of Int64.t
  | `UInt64 of Unsigned.UInt64.t
  | `Float of float
  | `String of string
  | `Bytes of bytes
  | `Time of Time.t
  ]

type t =
  { result : Bind.t (* Retain ownership of the binding arrays and buffers. *)
  ; pointer : T.Field.t ptr
  ; null_pointer : char ptr
  ; length_pointer : Unsigned.ulong ptr
  ; buffer_pointer : unit ptr ptr
  ; typ : Bind.buffer_type
  ; unsigned : bool
  }

let create result pointer at =
  (* Type and signedness are fixed for this result column. Cache only addresses
     for row-specific cells, whose contents must still be read on each row. *)
  let binding = result.Bind.bind +@ at in
  let view = !@binding in
  { result; pointer
  ; null_pointer = result.Bind.is_null +@ at
  ; length_pointer = result.Bind.length +@ at
  ; buffer_pointer = binding |-> T.Bind.buffer
  ; typ = Bind.buffer_type_of_int (getf view T.Bind.buffer_type)
  ; unsigned = getf view T.Bind.is_unsigned = '\001'
  }

let name field =
  getf (!@(field.pointer)) T.Field.name

let null_value field =
  !@(field.null_pointer) = '\001'

let can_be_null field =
  let flags = getf (!@(field.pointer)) T.Field.flags in
  Unsigned.UInt.logand flags T.Field.Flags.not_null = Unsigned.UInt.zero

let buffer field =
  !@(field.buffer_pointer)

let cast_to typ field =
  (* The buffer is already a void pointer: avoid constructing a general-purpose
     Ctypes coercion for each scalar read. *)
  !@(from_voidp typ (buffer field))

let to_string field =
  let length = Unsigned.ULong.to_int !@(field.length_pointer) in
  match length with
  | 0 -> ""
  | _ ->
    let p = from_voidp char (buffer field) in
    string_from_ptr p ~length

let to_bytes field =
  match to_string field with
  | "" -> Bytes.empty
  | s -> Bytes.unsafe_of_string s

let to_time field kind =
  let buf = buffer field in
  let tp = from_voidp T.Time.t buf in
  let member f = Unsigned.UInt.to_int @@ getf (!@tp) f in
  let member_long f = Unsigned.ULong.to_int @@ getf (!@tp) f in
  { Time.
    year   = member T.Time.year
  ; month  = member T.Time.month
  ; day    = member T.Time.day
  ; hour   = member T.Time.hour
  ; minute = member T.Time.minute
  ; second = member T.Time.second
  ; microsecond = member_long T.Time.second_part
  ; kind
  }

type to_string = [`Decimal | `New_decimal | `String | `Var_string | `Bit]
type to_blob   = [`Tiny_blob | `Blob | `Medium_blob | `Long_blob | `Json]
type to_time   = [`Time | `Date | `Datetime | `Timestamp]
(* MariaDB implements the JSON datatype as an alias for LONGTEXT.  It's
 * therefore * included it in to_blob above, so that the representation is
 * consitent in the public API. *)

let convert field typ unsigned =
  let open Signed in
  let open Unsigned in
  match typ, unsigned with
  | `Null,                _ -> `Null
  | `Year,                _
  | `Tiny,             true -> `Int (int_of_char (cast_to char field))
  | `Tiny,            false -> `Int (cast_to schar field)
  | `Short,            true -> `Int (cast_to int field)
  | `Short,           false -> `Int (UInt.to_int (cast_to uint field))
  | (`Int24 | `Long),  true -> `Int (UInt32.to_int (cast_to uint32_t field))
  | (`Int24 | `Long), false -> `Int (Int32.to_int (cast_to int32_t field))
  | `Long_long,        true -> `UInt64 (cast_to uint64_t field)
  | `Long_long,       false -> `Int64 (cast_to int64_t field)
  | `Float,               _ -> `Float (cast_to float field)
  | `Double,              _ -> `Float (cast_to double field)
  | #to_string,           _ -> `String (to_string field)
  | #to_blob,             _ -> `Bytes (to_bytes field)
  | #to_time as t,        _ -> `Time (to_time field t)

let value field =
  if null_value field then `Null
  else convert field field.typ field.unsigned

let err field ~info =
  failwith @@ "field '" ^ name field ^ "' is not " ^ info

let int field =
  match value field with
  | `Int i -> i
  | `Int64 i -> Int64.to_int i
  | `UInt64 i -> Unsigned.UInt64.to_int i
  | _ -> err field ~info:"an integer"

let int64 field =
  match value field with
  | `Int i -> Int64.of_int i
  | `Int64 i -> i
  | _ -> err field ~info:"a 64-bit integer"

let uint64 field =
  match value field with
  | `UInt64 i -> i
  | _ -> err field ~info:"a 64-bit unsigned integer"

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

let int_opt field =
  match value field with
  | `Int i -> Some i
  | `Int64 i -> Some (Int64.to_int i)
  | `UInt64 i -> Some (Unsigned.UInt64.to_int i)
  | `Null -> None
  | _ -> err field ~info:"a nullable integer"

let int64_opt field =
  match value field with
  | `Int i -> Some (Int64.of_int i)
  | `Int64 i -> Some i
  | `Null -> None
  | _ -> err field ~info:"a nullable 64-bit integer"

let uint64_opt field =
  match value field with
  | `UInt64 i -> Some i
  | `Null -> None
  | _ -> err field ~info:"a nullable 64-bit unsigned integer"

let float_opt field =
  match value field with
  | `Float x -> Some x
  | `Null -> None
  | _ -> err field ~info:"a nullable float"

let string_opt field =
  match value field with
  | `String s -> Some s
  | `Null -> None
  | _ -> err field ~info:"a nullable string"

let bytes_opt field =
  match value field with
  | `Bytes b -> Some b
  | `Null -> None
  | _ -> err field ~info:"a nullable byte string"

let time_opt field =
  match value field with
  | `Time t -> Some t
  | `Null -> None
  | _ -> err field ~info:"a nullable time value"
