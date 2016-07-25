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
  type t

  val create : ([< mode], [< state]) mariadb -> t
  val errno : t -> int
  val message : t -> string
end

module Res : sig
  type 'm t constraint 'm = [< mode]

  val num_fields : [< mode] t -> int
  val num_rows : [< mode] t -> int
  val free : [< mode] t -> unit
end

module Stmt : sig
  type 'm t constraint 'm = [< mode]

  val init : ('m, [`Connected]) mariadb -> 'm t option

  module Error : sig
    type 'm stmt = 'm t
    type t

    val create : 'm stmt -> t
    val errno : t -> int
    val message : t -> string
  end
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

  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Error.t]

  module Res : sig
    type t = [`Nonblocking] Res.t

    val fetch_row_start : t -> [`Ok of row | `Wait of Status.t | `Done]
    val fetch_row_cont : t -> Status.t
                      -> [`Ok of row | `Wait of Status.t | `Done]

    val free_start : t -> [`Ok | `Wait of Status.t]
    val free_cont : t -> Status.t -> [`Ok | `Wait of Status.t]
  end

  module Stmt : sig
    type t = [`Nonblocking] Stmt.t

    val prepare_start : t -> string -> unit result
    val prepare_cont : t -> Status.t -> unit result

    val execute_start : t -> string -> unit result
    val execute_cont : t -> Status.t -> unit result

    val fetch_start : t -> string -> unit result
    val fetch_cont : t -> Status.t -> unit result

    val store_result_start : t -> string -> unit result
    val store_result_cont : t -> Status.t -> unit result

    val close_start : t -> string -> unit result
    val close_cont : t -> Status.t -> unit result

    val reset_start : t -> string -> unit result
    val reset_cont : t -> Status.t -> unit result

    val next_result_start : t -> bool result
    val next_result_cont : t -> Status.t -> bool result
  end

  module Tx : sig
    val commit_start : [`Connected] t -> [`Tx] t result
    val commit_cont : [`Connected] t -> Status.t -> [`Tx] t result

    val rollback_start : [`Tx] t -> [`Connected] t result
    val rollback_cont : [`Tx] t -> Status.t ->  [`Connected] t result

    val autocommit_start : [`Connected] t -> bool -> [`Connected] t result
    val autocommit_cont : [`Connected] t -> Status.t -> [`Connected] t result
  end

  val init : unit -> [`Initialized] t option
  val close_start : [`Connected] t -> [`Ok | `Wait of Status.t]
  val close_cont : [`Connected] t -> Status.t -> [`Ok | `Wait of Status.t]

  val connect_start : [`Initialized] t
                   -> ?host:string
                   -> ?user:string
                   -> ?pass:string
                   -> ?db:string -> ?port:int -> ?socket:string
                   -> ?flags:flag list -> unit
                   -> [`Connected] t result

  val connect_cont : [`Initialized] t -> Status.t
                  -> [`Connected] t result

  val query_start : [`Connected] t -> string -> unit result
  val query_cont : [`Connected] t -> Status.t -> unit result

  val fd : [< `Initialized | `Connected] t -> int
  val timeout : [< `Initialized | `Connected] t -> int

  val set_charset_start : [`Connected] t -> string -> unit result
  val set_charset_cont : [`Connected] t -> Status.t -> unit result

  val select_db_start : [`Connected] t -> string -> unit result
  val select_db_cont : [`Connected] t -> Status.t -> unit result

  val change_user_start : [`Connected] t -> string -> string -> string option
                       -> unit result
  val change_user_cont : [`Connected] t -> Status.t -> unit result

  val dump_debug_info_start : [`Connected] t -> unit result
  val dump_debug_info_cont : [`Connected] t -> Status.t -> unit result

  val set_server_option_start : [`Connected] t -> server_option -> unit result
  val set_server_option_cont : [`Connected] t -> Status.t -> unit result

  val list_dbs_start : [`Connected] t -> string -> Res.t result
  val list_dbs_cont : [`Connected] t -> Status.t -> Res.t result

  val list_tables_start : [`Connected] t -> string -> Res.t result
  val list_tables_cont : [`Connected] t -> Status.t -> Res.t result

  val next_result_start : [`Connected] t -> bool result
  val next_result_cont : [`Connected] t -> Status.t -> bool result
end

val init : unit -> ([`Blocking], [`Initialized]) t option
val close : ([< mode], [`Connected]) t -> unit
val use_result: ([< mode], [`Connected]) t -> [< mode] Res.t option
