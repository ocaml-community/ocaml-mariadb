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

    val num_rows : t -> int
    val affected_rows : t -> int
    val fetch : (module Row.S with type t = 'r) -> t -> 'r option result
    val stream : (module Row.S with type t = 'r) -> t -> 'r Stream.t result
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
    | Client_can_handle_expired_passwords
    | Compress
    | Found_rows
    | Ignore_sigpipe
    | Ignore_space
    | Interactive
    | Local_files
    | Multi_results
    | Multi_statements
    | No_schema
    | Odbc
    | Ssl
    | Remember_options

  type protocol =
    | Default
    | Tcp
    | Socket
    | Pipe
    | Memory

  type client_option =
    | Connect_timeout of int
    | Compress
    | Named_pipe of string
    | Init_command of string
    | Read_default_file of string
    | Read_default_group of string
    | Set_charset_dir of string
    | Set_charset_name of string
    | Local_infile of bool
    | Protocol of protocol
    | Shared_memory_base_name of string
    | Read_timeout of int
    | Write_timeout of int
    | Secure_auth of bool
    | Report_data_truncation of bool
    | Reconnect of bool
    | Ssl_verify_server_cert of bool
    | Plugin_dir of string
    | Default_auth of string
    | Bind of string
    | Ssl_key of string
    | Ssl_cert of string
    | Ssl_ca of string
    | Ssl_capath of string
    | Ssl_cipher of string
    | Ssl_crl of string
    | Ssl_crlpath of string
    | Connect_attr_reset
    | Connect_attr_add of string * string
    | Connect_attr_delete of string
    | Server_public_key of string
    | Enable_cleartext_plugin of bool
    | Can_handle_expired_passwords of bool
    | Use_thread_specific_memory of bool

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
  val set_client_option : t -> client_option -> unit
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
