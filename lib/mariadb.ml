module type S = sig
  type error = int * string
  type 'a result = ('a, Common.error) Pervasives.result

  module Field : sig
    type t

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

    val name : t -> string
    val value : t -> value
    val null_value : t -> bool
    val can_be_null : t -> bool

    val int : t -> int
    val float : t -> float
    val string : t -> string
    val bytes : t -> bytes
    val time : t -> time

    val null_int : t -> int option
    val null_float : t -> float option
    val null_string : t -> string option
    val null_bytes : t -> bytes option
    val null_time : t -> time option
  end

  module Row : sig
    module type S = sig
      type t
      val build : int -> (int -> Field.t) -> t
    end

    module StringMap : Map.S with type key = string

    module Array : (S with type t = Field.t array)
    module Map : (S with type t = Field.t StringMap.t)
    module Hashtbl : (S with type t = (string, Field.t) Hashtbl.t)
  end

  module Res : sig
    type t

    val fetch : (module Row.S with type t = 'r) -> t -> 'r option result
    val stream : (module Row.S with type t = 'r) -> t -> 'r Stream.t result
    val num_rows : t -> int
  end

  module Stmt : sig
    type t

    type param =
      [ `Tiny of int
      | `Short of int
      | `Int of int
      | `Float of float
      | `Double of float
      | `String of string
      | `Blob of bytes
      ]

    val execute : t -> param array -> Res.t result
    val close : t -> unit result
  end

  type t

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

  val connect : ?host:string
             -> ?user:string
             -> ?pass:string
             -> ?db:string -> ?port:int -> ?socket:string
             -> ?flags:flag list -> unit
             -> t result

  val close : t -> unit

  val set_character_set : t -> string -> unit result
  val select_db : t -> string -> unit result
  val change_user : t -> string -> string -> string option -> unit result
  val dump_debug_info : t -> unit result
  val set_server_option : t -> server_option -> unit result
  val ping : t -> unit result
  val autocommit : t -> bool -> unit result
  val prepare : t -> string -> Stmt.t result
end

module B = Ffi_bindings.Bindings(Ffi_generated)

module Common = Common
module Blocking = Blocking
module Nonblocking = Nonblocking

let () =
  match B.mysql_library_init 0 None None with
  | 0 -> at_exit B.mysql_library_end
  | _ -> failwith "cannot initialize MariaDB library"
