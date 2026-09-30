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
  { result : Bind.t
  ; pointer : T.Field.t ptr
  ; null_pointer : char ptr
  ; read : unit -> value
  }

let name field =
  getf (!@(field.pointer)) T.Field.name

let null_value field =
  !@(field.null_pointer) = '\001'

let can_be_null field =
  let flags = getf (!@(field.pointer)) T.Field.flags in
  Unsigned.UInt.logand flags T.Field.Flags.not_null = Unsigned.UInt.zero

let cast_to typ buffer_pointer = !@(from_voidp typ !@buffer_pointer)

let to_string buffer_pointer length_pointer =
  let length = Unsigned.ULong.to_int !@length_pointer in
  if length = 0 then ""
  else string_from_ptr (from_voidp char !@buffer_pointer) ~length

let to_bytes buffer_pointer length_pointer =
  match to_string buffer_pointer length_pointer with
  | "" -> Bytes.empty
  | s -> Bytes.unsafe_of_string s

let to_time buffer_pointer kind =
  let tp = from_voidp T.Time.t !@buffer_pointer in
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

let decoder buffer_pointer length_pointer typ unsigned =
  let open Signed in
  let open Unsigned in
  match typ, unsigned with
  | `Null,                _ -> (fun () -> `Null)
  | `Year,                _
  | `Tiny,             true -> (fun () -> `Int (int_of_char (cast_to char buffer_pointer)))
  | `Tiny,            false -> (fun () -> `Int (cast_to schar buffer_pointer))
  | `Short,            true -> (fun () -> `Int (cast_to int buffer_pointer))
  | `Short,           false -> (fun () -> `Int (UInt.to_int (cast_to uint buffer_pointer)))
  | (`Int24 | `Long),  true -> (fun () -> `Int (UInt32.to_int (cast_to uint32_t buffer_pointer)))
  | (`Int24 | `Long), false -> (fun () -> `Int (Int32.to_int (cast_to int32_t buffer_pointer)))
  | `Long_long,        true -> (fun () -> `UInt64 (cast_to uint64_t buffer_pointer))
  | `Long_long,       false -> (fun () -> `Int64 (cast_to int64_t buffer_pointer))
  | `Float,               _ -> (fun () -> `Float (cast_to float buffer_pointer))
  | `Double,              _ -> (fun () -> `Float (cast_to double buffer_pointer))
  | #to_string,           _ -> (fun () -> `String (to_string buffer_pointer length_pointer))
  | #to_blob,             _ -> (fun () -> `Bytes (to_bytes buffer_pointer length_pointer))
  | #to_time as kind,     _ -> (fun () -> `Time (to_time buffer_pointer kind))

let create result pointer at =
  let binding = result.Bind.bind +@ at in
  let view = !@binding in
  let null_pointer = result.Bind.is_null +@ at in
  let length_pointer = result.Bind.length +@ at in
  let buffer_pointer = binding |-> T.Bind.buffer in
  let typ = Bind.buffer_type_of_int (getf view T.Bind.buffer_type) in
  let unsigned = getf view T.Bind.is_unsigned = '\001' in
  let decode = decoder buffer_pointer length_pointer typ unsigned in
  { result; pointer; null_pointer; read = decode }

let value field =
  if null_value field then `Null else field.read ()

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
