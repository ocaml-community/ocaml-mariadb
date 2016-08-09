type mode = [`Blocking | `Nonblocking]
type error = int * string

module Row = Row
module Field = Field
module StringMap = Row.StringMap

module type S = sig
  type error = int * string
  type 'a result = ('a, error) Pervasives.result

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

  type flag
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

module B = Ffi_bindings.Bindings(Ffi_generated)

module Common = Common
module Blocking = Blocking
module Nonblocking = Nonblocking

let () =
  match B.mysql_library_init 0 None None with
  | 0 -> at_exit B.mysql_library_end
  | _ -> failwith "cannot initialize MariaDB library"
