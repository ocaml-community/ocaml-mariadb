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

module Make (W : Wait) : (module type of Mariadb)
