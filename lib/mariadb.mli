type error = int * string

(*module Blocking : Mariadb_intf.S*)

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

    val free : t -> (unit -> [`Ok | `Wait of Status.t]) *
                    (Status.t -> [`Ok | `Wait of Status.t])
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

  val set_charset : [`Connected] t -> string -> unit nonblocking
  val select_db : [`Connected] t -> string -> unit nonblocking
  val change_user : [`Connected] t -> string -> string -> string option
                 -> unit nonblocking
  val dump_debug_info : [`Connected] t -> unit nonblocking
  val set_server_option : [`Connected] t -> Common.server_option
                       -> unit nonblocking
  val ping : [`Connected] t -> unit nonblocking

  (*val list_dbs_start : [`Connected] t -> string -> Res.t result
  val list_dbs_cont : [`Connected] t -> Status.t -> Res.t result

  val list_tables_start : [`Connected] t -> string -> Res.t result
  val list_tables_cont : [`Connected] t -> Status.t -> Res.t result

  val next_result_start : [`Connected] t -> bool result
  val next_result_cont : [`Connected] t -> Status.t -> bool result*)

  val prepare : [`Connected] t -> string
             -> [ `Ok of ([`Prepared] Stmt.t nonblocking) | `Error of error]

  module type Wait = sig
    val wait : [< `Connected | `Tx] t -> Status.t -> Status.t
  end

  module Make (W : Wait) : Mariadb_intf.S
end
