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
  { bind : T.Bind.t ptr
  ; field : T.Field.t ptr
  ; value : value
  }

let create bind field value =
  { bind; field; value }

let name f =
  getf (!@(f.field)) T.Field.name

let value f =
  f.value
