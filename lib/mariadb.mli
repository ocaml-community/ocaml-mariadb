module type S = sig
  type error = int * string
  type 'a result = ('a, error) Pervasives.result

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
    type state = [`Prepared | `Executed]
    type 's t constraint 's = [< state]

    type param =
      [ `Tiny of int
      | `Short of int
      | `Int of int
      | `Float of float
      | `Double of float
      | `String of string
      | `Blob of bytes
      ]

    val execute : [`Prepared] t -> param array -> Res.t result
    val close : [< state] t -> unit result
  end

  type state = [`Unconnected | `Connected | `Tx]
  type 's t constraint 's = [< state]

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
             -> [`Connected] t result

  val close : [< `Connected | `Tx] t -> unit

  val set_character_set : [`Connected] t -> string -> unit result
  val select_db : [`Connected] t -> string -> unit result
  val change_user : [`Connected] t -> string -> string -> string option
                 -> unit result
  val dump_debug_info : [`Connected] t -> unit result
  val set_server_option : [`Connected] t -> server_option
                       -> unit result
  val ping : [`Connected] t -> unit result
  val prepare : [`Connected] t -> string -> [`Prepared] Stmt.t result

  module Tx : sig
    val commit : [`Connected] t -> [`Tx] t result
    val rollback : [`Tx] t -> [`Connected] t result
    val autocommit : [`Connected] t -> bool -> [`Connected] t result
  end
end

module Blocking : S

module Nonblocking : sig
  module Status : sig
    type t

    val create : ?read:bool
              -> ?write:bool
              -> ?except:bool
              -> ?timeout:bool
              -> unit -> t
    val of_int : int -> t
    val to_int : t -> int
    val read : t -> bool
    val write : t -> bool
    val except : t -> bool
    val timeout : t -> bool
  end

  type 's t = ([`Nonblocking], 's) Common.t

  val fd : [< `Unconnected | `Connected] t -> int
  val timeout : [< `Unconnected | `Connected] t -> int

  module type Wait = sig
    val wait : [< `Connected | `Tx] t -> Status.t -> Status.t
  end

  module Make (W : Wait) : S
end
