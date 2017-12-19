(** OCaml-MariaDB is a library with OCaml bindings for MariaDB's
    libmysqlclient, including support for its nonblocking API. While you
    need MariaDB's libmysqlclient to build OCaml-MariaDB, it should be
    possible to use it with the regular libmysqlclient from MySQL,
    as long as you don't try to use the nonblocking API.

    These bindings are restricted to MariaDB's prepared statement APIs,
    as they provide support for typed query parameters and results.
*)

(** The MariaDB blocking interface. *)
module type S = sig
  type error = int * string
    (** The type of errors that can result from MariaDB API calls. *)

  type 'a result = ('a, error) Pervasives.result
    (** The result of MariaDB API calls. *)

  type t
    (** The type of database handles. *)

  val connect : ?host:string
             -> ?user:string
             -> ?pass:string
             -> ?db:string -> ?port:int -> ?socket:string
             -> unit
             -> t result
		(** Connect to a MariaDB server at the specified location with the specified
				flags and optionally select a database [db]. *)

  val close : t -> unit
    (** Close a database handle. *)

end

(** This is the nonblocking MariaDB API. The interface contains a functor
    [Make] which, given a way to wait for a connection socket to be ready
    for reading or writing, returns a module with the same signature [S]
    as the traditional blocking API. *)
module Nonblocking : sig
  module Status : sig
    type t
      (** The type of a nonblocking operation status. *)

    val create : ?read:bool
              -> ?write:bool
              -> ?except:bool
              -> ?timeout:bool
              -> unit -> t
      (** Create a new status indicating which events have occured on the
          MariaDB connection socket. *)

    val read : t -> bool
      (** Indicates if a read event has occurred. *)

    val write : t -> bool
      (** Indicates if a write event has occurred. *)

    val except : t -> bool
      (** Indicates if an exceptional condition event has occurred. *)

    val timeout : t -> bool
      (** Indicates if a timeout has occurred. *)
  end

  type t
    (** The type of nonblocking database handles. *)

  val fd : t -> Unix.file_descr
    (** The underlying file descriptor of the database connection. *)

  val timeout : t -> int
    (** If a nonblocking operation returns a [Status.t] indicating a timeout
        event, this function can be used to obtain the value, in seconds,
        after which the timeout has occured. *)

	(** Input module signature for the functor that generates a nonblocking
			connection module. *)
  module type Wait = sig
    (** A module defining a nonblocking I/O monadic interface. *)
    module IO : sig
      type 'a future

      val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
      val return : 'a -> 'a future
    end

    val wait : t -> Status.t -> Status.t IO.future
			(** [wait mariadb status] must wait for the events set in [status]
					to occur in the [mariadb] connection and return a [Status.t]
					indicating which events have actually occured. *)
  end

  (* The MariaDB nonblocking interface. The exact same functions in the
     blocking interface are available here, but operations that could
     otherwise block there return a [future] value here. *)
  module type S = sig
    type error = int * string
    type 'a future
    type 'a result = ('a, error) Pervasives.result

    type t

    val connect : ?host:string
               -> ?user:string
               -> ?pass:string
               -> ?db:string -> ?port:int -> ?socket:string
               -> unit
               -> t result future

    val close : t -> unit future
  end

	(** Functor that generates a nonblocking database interface, given a
      nonblocking IO monad and a way to wait for connection socket events. *)
  module Make (W : Wait) : S with type 'a future := 'a W.IO.future
end
