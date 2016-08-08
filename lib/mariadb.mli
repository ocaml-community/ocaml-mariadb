type mode = [`Blocking | `Nonblocking]
type error = int * string

module type S = sig
  type error = int * string
  type 'a result = ('a, error) Pervasives.result

  module Res : sig
    type t

    type time = Common.Res.time =
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

    val fetch : t -> value array option result
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

    val execute' : [`Prepared] t -> param array
                -> ([`Executed] t * Res.t) result

    val close : [< state] t -> unit result

    (*val reset : [`Executed] t -> [`Prepared] t result*)
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

  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

  type 'a start = unit -> 'a result
  type 'a cont = Status.t -> 'a result
  type 'a nonblocking = 'a start * 'a cont

  module Res : sig
    type t = [`Nonblocking] Common.Res.t

    type time = Common.Res.time =
      { year : int
      ; month : int
      ; day : int
      ; hour : int
      ; minute : int
      ; second : int
      }

    val fetch : t -> Common.Res.value array option nonblocking

    val num_rows : t -> int

    val free : t -> unit nonblocking
  end

  module Stmt : sig
    type 's t = ([`Nonblocking], 's) Common.Stmt.t

    type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

    val execute : [`Prepared] t -> Common.Stmt.param array
               -> [ `Ok of ([`Executed] t nonblocking) | `Error of error]

    val store_result : [`Executed] t -> Res.t nonblocking

    val close : [< Common.Stmt.state] t -> unit nonblocking

    val reset : [`Executed] t -> [`Prepared] t nonblocking

    (*val next_result_start : t -> bool result
    val next_result_cont : t -> Status.t -> bool result*)
  end

  module Tx : sig
    val commit : [`Connected] t -> [`Tx] t nonblocking
    val rollback : [`Tx] t -> [`Connected] t nonblocking
    val autocommit : [`Connected] t -> bool -> [`Connected] t nonblocking
  end

  val init : unit -> [`Unconnected] t option

  val connect : [`Unconnected] t
             -> ?host:string
             -> ?user:string
             -> ?pass:string
             -> ?db:string -> ?port:int -> ?socket:string
             -> ?flags:Common.flag list -> unit
             -> [`Connected] t nonblocking

  val close : [`Connected | `Tx] t
           -> (unit -> [`Ok | `Wait of Status.t]) *
              (Status.t -> [`Ok | `Wait of Status.t])

  val fd : [< `Unconnected | `Connected] t -> int
  val timeout : [< `Unconnected | `Connected] t -> int

  val set_character_set : [`Connected] t -> string -> unit nonblocking
  val select_db : [`Connected] t -> string -> unit nonblocking
  val change_user : [`Connected] t -> string -> string -> string option
                 -> unit nonblocking
  val dump_debug_info : [`Connected] t -> unit nonblocking
  val set_server_option : [`Connected] t -> Common.server_option
                       -> unit nonblocking
  val ping : [`Connected] t -> unit nonblocking

  val prepare : [`Connected] t -> string
             -> [ `Ok of ([`Prepared] Stmt.t nonblocking) | `Error of error]

  module type Wait = sig
    val wait : [< `Connected | `Tx] t -> Status.t -> Status.t
  end

  module Make (W : Wait) : S
end
