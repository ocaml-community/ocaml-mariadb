module B = Binding_wrappers
module T = Ffi_bindings.Types(Ffi_generated_types)

module Status = Wait_status

type mode = [`Blocking | `Nonblocking]
type 'm connection = B.mysql constraint 'm = [< mode]

type t = [`Nonblocking] connection
type mariadb = t

type error = int * string
type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

let error mariadb =
	(B.mysql_errno mariadb, B.mysql_error mariadb)

let init () =
  match B.mysql_init () with
  | Some m ->
      B.mysql_options m T.Options.nonblock Ctypes.null |> ignore;
      Some m
  | None ->
      None

let handle_void = function
  | 0 -> `Ok
  | s -> `Wait (Status.of_int s)

let handle_opt mariadb = function
  | 0, Some r -> `Ok mariadb
  | 0, None -> `Error (error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let connect_start mariadb host user pass db port socket () =
  handle_opt mariadb
    (B.mysql_real_connect_start mariadb host user pass db port socket 0)

let connect_cont mariadb status =
  handle_opt mariadb
		(B.mysql_real_connect_cont mariadb (Status.to_int status))

let connect mariadb ?host ?user ?pass ?db ?(port=0) ?socket () =
  let start = connect_start mariadb host user pass db port socket in
  let cont = connect_cont mariadb in
  (start, cont)

let close_start mariadb () =
  handle_void (B.mysql_close_start mariadb)

let close_cont mariadb status =
  handle_void (B.mysql_close_cont mariadb status)

let close mariadb =
  (close_start mariadb, close_cont mariadb)

let fd mariadb =
  Obj.magic @@ B.mysql_get_socket mariadb

let timeout mariadb =
  B.mysql_get_timeout_value mariadb

module type Wait = sig
  module IO : sig
    type 'a future
    val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
    val return : 'a -> 'a future
  end

  val wait : t -> Status.t -> Status.t IO.future
end

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

module Make (W : Wait) : S with type 'a future = 'a W.IO.future = struct
  type t = mariadb

  type 'a future = 'a W.IO.future
  type error = int * string
  type 'a result = ('a, error) Pervasives.result

  let (>>=) = W.IO.(>>=)
  let return = W.IO.return
  let return_unit = return ()
  let (>>|) fut f = fut >>= fun x -> return (f x)

  let rec nonblocking m (f, g) =
    match f () with
    | `Ok v -> return (Ok v)
    | `Wait s -> W.wait m s >>= fun s -> nonblocking m ((fun () -> g s), g)
    | `Error e -> return (Error e)

  let rec nonblocking' m (f, g) =
    match f () with
    | `Ok -> return_unit
    | `Wait s -> W.wait m s >>= fun s -> nonblocking' m ((fun () -> g s), g)

  let connect ?host ?user ?pass ?db ?(port=0) ?socket () =
    match init () with
    | Some m ->
        nonblocking m (connect m ?host ?user ?pass ?db ~port ?socket ())
    | None ->
        return (Error (2008, "out of memory"))

  let close m = nonblocking' m (close m)
end
