type mode = [`Blocking | `Nonblocking]

type 'm t constraint 'm = [< mode]
type nonblocking = [`Nonblocking] t

type 'm res constraint 'm = [< mode]
type nonblocking_res = [`Nonblocking] res

type row = string array

type 'm stmt constraint 'm = [< mode]

type flag

type server_option =
  | Multi_statements of bool

type error = int * string

val init : ?mariadb:([`Blocking] t) -> unit -> [`Blocking] t option
val close : [< mode] t -> unit
val use_result : [< mode] t -> [< mode] res option
val errno : [< mode] t -> int
val error : [< mode] t -> string

val num_fields : [< mode] res -> int
val num_rows : [< mode] res -> int
val free_result : [< mode] res -> unit

val stmt_init : 'm t -> 'm stmt option

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

  type t = nonblocking
  type res = nonblocking_res

  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

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

  val free_result_start : res -> [`Ok | `Wait of Status.t]
  val free_result_cont : res -> Status.t -> [`Ok | `Wait of Status.t]

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

  val stmt_prepare_start : t -> string -> unit result
  val stmt_prepare_cont : t -> Status.t -> unit result

  val stmt_execute_start : t -> string -> unit result
  val stmt_execute_cont : t -> Status.t -> unit result

  val stmt_fetch_start : t -> string -> unit result
  val stmt_fetch_cont : t -> Status.t -> unit result

  val stmt_store_result_start : t -> string -> unit result
  val stmt_store_result_cont : t -> Status.t -> unit result

  val stmt_close_start : t -> string -> unit result
  val stmt_close_cont : t -> Status.t -> unit result

  val stmt_reset_start : t -> string -> unit result
  val stmt_reset_cont : t -> Status.t -> unit result

  val commit_start : t -> unit result
  val commit_cont : t -> Status.t -> unit result

  val rollback_start : t -> unit result
  val rollback_cont : t -> Status.t -> unit result

  val autocommit_start : t -> bool -> unit result
  val autocommit_cont : t -> Status.t -> unit result

  val next_result_start : t -> bool result
  val next_result_cont : t -> Status.t -> bool result

  val stmt_next_result_start : [`Nonblocking] stmt -> bool result
  val stmt_next_result_cont : [`Nonblocking] stmt -> Status.t -> bool result
end
