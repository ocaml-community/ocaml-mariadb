type mode = [`Blocking | `Nonblocking]
type state = [`Initialized | `Connected | `Tx]

type ('m, 's) t
  constraint 'm = [< mode]
  constraint 's = [< state]
type ('m, 's) mariadb = ('m, 's) t

type row = string array

type flag

type server_option =
  | Multi_statements of bool

module Error : sig
  type t = int * string

  val create : ([< mode], [< state]) mariadb -> t
  val errno : t -> int
  val message : t -> string
end

module Res : sig
  type 'm t constraint 'm = [< mode]

  val num_rows : [< mode] t -> int
  val free : [< mode] t -> unit
end

module Stmt : sig
  type state = [`Prepared | `Bound | `Executed | `Stored | `Fetch]

  type ('m, 's) t
    constraint 'm = [< mode]
    constraint 's = [< state]

  type param =
    [ `Tiny of int
    | `Short of int
    | `Int of int
    | `Float of float
    | `Double of float
    | `String of string
    | `Blob of bytes
    ]

  module Error : sig
    type ('m, 's) stmt = ('m, 's) t
    type t = int * string

    val create : ('m, 's) stmt -> t
    val errno : t -> int
    val message : t -> string
  end

  type 'a result = [`Ok of 'a | `Error of Error.t]

  val bind_params : ([< mode], [`Prepared]) t -> param array
                 -> ([< mode], [`Bound]) t result
end

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

  type 's t = ([`Nonblocking], 's) mariadb
  type 's mariadb = 's t

  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Error.t]

  type 'a start = unit -> 'a result
  type 'a cont = Status.t -> 'a result
  type 'a nonblocking = 'a start * 'a cont

  module Res : sig
    type t = [`Nonblocking] Res.t
    type value =
      [ `Int of int
      | `Float of float
      | `String of string
      | `Bytes of bytes
      | `Null
      ]

    val fetch_row : t -> row option nonblocking

    val fetch : t -> value array option nonblocking

    val free : t -> (unit -> [`Ok | `Wait of Status.t]) *
                    (Status.t -> [`Ok | `Wait of Status.t])
  end

  module Stmt : sig
    type 's t = ([`Nonblocking], 's) Stmt.t
      constraint 's = [< Stmt.state]

    type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Stmt.Error.t]

    val execute : [`Prepared] t -> Stmt.param array
               -> [ `Ok of ([`Executed] t nonblocking) | `Error of Stmt.Error.t]

    val store_result : [`Executed] t -> Res.t nonblocking

    val close : [< Stmt.state] t -> unit nonblocking

    val reset : [< Stmt.state] t -> unit nonblocking

    (*val next_result_start : t -> bool result
    val next_result_cont : t -> Status.t -> bool result*)
  end

  module Tx : sig
    val commit : [`Connected] t -> [`Tx] t nonblocking
    val rollback : [`Tx] t -> [`Connected] t nonblocking
    val autocommit : [`Connected] t -> bool -> [`Connected] t nonblocking
  end

  val init : unit -> [`Initialized] t option
  val close : [`Connected] t
           -> (unit -> [`Ok | `Wait of Status.t]) *
              (Status.t -> [`Ok | `Wait of Status.t])

  val connect : [`Initialized] t
            -> ?host:string
            -> ?user:string
            -> ?pass:string
            -> ?db:string -> ?port:int -> ?socket:string
            -> ?flags:flag list -> unit
            -> [`Connected] t nonblocking

  val fd : [< `Initialized | `Connected] t -> int
  val timeout : [< `Initialized | `Connected] t -> int

  val set_charset : [`Connected] t -> string -> unit nonblocking
  val select_db : [`Connected] t -> string -> unit nonblocking
  val change_user : [`Connected] t -> string -> string -> string option
                 -> unit nonblocking
  val dump_debug_info : [`Connected] t -> unit nonblocking
  val set_server_option : [`Connected] t -> server_option -> unit nonblocking
  val ping : [`Connected] t -> unit nonblocking

  (*val list_dbs_start : [`Connected] t -> string -> Res.t result
  val list_dbs_cont : [`Connected] t -> Status.t -> Res.t result

  val list_tables_start : [`Connected] t -> string -> Res.t result
  val list_tables_cont : [`Connected] t -> Status.t -> Res.t result

  val next_result_start : [`Connected] t -> bool result
  val next_result_cont : [`Connected] t -> Status.t -> bool result*)

  val prepare : [`Connected] t -> string
             -> [ `Ok of ([`Prepared] Stmt.t nonblocking) | `Error of Error.t]
end

val init : unit -> ([`Blocking], [`Initialized]) t option
val close : ([< mode], [`Connected]) t -> unit
(* val use_result: ([< mode], [`Connected]) t -> [< mode] Res.t option *)
