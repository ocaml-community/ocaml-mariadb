type mode = [`Blocking | `Nonblocking]

type 'm t constraint 'm = [< mode]
type 'm mariadb = 'm t

type row = string array

type flag

type server_option =
  | Multi_statements of bool

module Error : sig
  type t

  val create : [< mode] mariadb -> t
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

  val init : 'm mariadb -> 'm t option

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

  type t = [`Nonblocking] mariadb
  type res = [`Nonblocking] Res.t
  type stmt = [`Nonblocking] Stmt.t

  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of Error.t]

  val init : ?mariadb:t -> unit -> t option
  val close_start : t -> [`Ok | `Wait of Status.t]
  val close_cont : t -> Status.t -> [`Ok | `Wait of Status.t]

  val connect_start : t -> ?host:string -> ?user:string -> ?pass:string
                   -> ?db:string -> ?port:int -> ?socket:string
                   -> ?flags:flag list -> unit
                   -> unit result

  val connect_cont : t -> Status.t
                  -> unit result

  val query_start : t -> string -> unit result
  val query_cont : t -> Status.t -> unit result

  val fetch_row_start : res -> [`Ok of row | `Wait of Status.t | `Done]
  val fetch_row_cont : res -> Status.t
                    -> [`Ok of row | `Wait of Status.t | `Done]

  val fd : t -> int
  val timeout : t -> int

  val set_charset_start : t -> string -> unit result
  val set_charset_cont : t -> Status.t -> unit result

  val select_db_start : t -> string -> unit result
  val select_db_cont : t -> Status.t -> unit result

  val change_user_start : t -> string -> string -> string option -> unit result
  val change_user_cont : t -> Status.t -> unit result

  val dump_debug_info_start : t -> unit result
  val dump_debug_info_cont : t -> Status.t -> unit result

  val set_server_option_start : t -> server_option -> unit result
  val set_server_option_cont : t -> Status.t -> unit result

  val list_dbs_start : t -> string -> res result
  val list_dbs_cont : t -> Status.t -> res result

  val list_tables_start : t -> string -> res result
  val list_tables_cont : t -> Status.t -> res result

  val next_result_start : t -> bool result
  val next_result_cont : t -> Status.t -> bool result

  module Res : sig
    val free_start : res -> [`Ok | `Wait of Status.t]
    val free_cont : res -> Status.t -> [`Ok | `Wait of Status.t]
  end

  module Stmt : sig
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

    val next_result_start : stmt -> bool result
    val next_result_cont : stmt -> Status.t -> bool result
  end

  module Tx : sig
    val commit_start : t -> unit result
    val commit_cont : t -> Status.t -> unit result

    val rollback_start : t -> unit result
    val rollback_cont : t -> Status.t -> unit result

    val autocommit_start : t -> bool -> unit result
    val autocommit_cont : t -> Status.t -> unit result
  end
end

val init : ?mariadb:([`Blocking] t) -> unit -> [`Blocking] t option
val close : [< mode] t -> unit
val use_result: [< mode] t -> [< mode] Res.t option
