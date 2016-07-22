type mode = [`Blocking | `Nonblocking]

type 'm t constraint 'm = [< mode]
type nonblocking = [`Nonblocking] t

type 'm res constraint 'm = [< mode]
type nonblocking_res = [`Nonblocking] res

type row = string array
type flag

type error = int * string


val init : ?mariadb:([`Blocking] t) -> unit -> [`Blocking] t option
val close : [< mode] t -> unit
val use_result : [< mode] t -> [< mode] res option
val errno : [< mode] t -> int
val error : [< mode] t -> string

val num_fields : [< mode] res -> int
val num_rows : [< mode] res -> int
val free_result : [< mode] res -> unit

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

  val init : ?mariadb:t -> unit -> t option
  val close_start : t -> [`Ok | `Wait of Status.t]
  val close_cont : t -> Status.t -> [`Ok | `Wait of Status.t]

  val connect_start : t -> ?host:string -> ?user:string -> ?pass:string
                   -> ?db:string -> ?port:int -> ?socket:string
                   -> ?flags:flag list -> unit
                   -> [`Ok | `Wait of Status.t | `Error of error]

  val connect_cont : t -> Status.t
                  -> [`Ok | `Wait of Status.t | `Error of error]

  val query_start : t -> string -> [`Ok | `Wait of Status.t | `Error of error]
  val query_cont : t -> Status.t -> [`Ok | `Wait of Status.t | `Error of error]

  val fetch_row_start : res -> [`Ok of row | `Wait of Status.t | `Done]
  val fetch_row_cont : res -> Status.t
                    -> [`Ok of row | `Wait of Status.t | `Done]

  val free_result_start : res -> [`Ok | `Wait of Status.t]
  val free_result_cont : res -> Status.t -> [`Ok | `Wait of Status.t]

  val fd : t -> int
  val timeout : t -> int
end
