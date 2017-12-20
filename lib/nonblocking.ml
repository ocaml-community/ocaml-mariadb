open Printf
open Util

module B = Binding_wrappers
module T = Ffi_bindings.Types(Ffi_generated_types)

module Time = Time
module Field = Field
module Row = Row
module Status = Wait_status

type t = [`Nonblocking] Common.t
type mariadb = t

type error = Common.error
type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

type server_option = Common.server_option

type 'a start = unit -> 'a result
type 'a cont = Status.t -> 'a result
type 'a nonblocking = 'a start * 'a cont

let map_result f = function
  | `Ok x -> `Ok (f x)
  | `Wait _ as w -> w
  | `Error _ as e -> e

type options =
  | Nonblocking

let options raw = function
  | Nonblocking ->
      B.mysql_options raw T.Options.nonblock Ctypes.null

let init () =
  match B.mysql_init () with
  | Some raw ->
      options raw Nonblocking;
      Some raw
  | None ->
      None

let handle_void f =
  match f () with
  | 0 -> `Ok
  | s -> `Wait (Status.of_int s)

let handle_opt mariadb f =
  match f mariadb.Common.raw with
  | 0, Some _ -> `Ok mariadb
  | 0, None -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_int_ret mariadb f =
  match f mariadb.Common.raw with
  | 0, 0 -> `Ok mariadb
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_int mariadb f =
  match f mariadb.Common.raw with
  | 0, 0 -> `Ok ()
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let handle_char mariadb f =
  match f mariadb.Common.raw with
  | 0, '\000' -> `Ok ()
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let connect_start mariadb () =
  handle_opt mariadb
    (fun m ->
      B.mysql_real_connect_start
        m
        mariadb.host
        mariadb.user
        mariadb.pass
        mariadb.db
        mariadb.port
        mariadb.socket
        mariadb.flags)

let connect_cont mariadb status =
  handle_opt mariadb
    (fun m -> B.mysql_real_connect_cont m (Status.to_int status))

let connect mariadb =
  (connect_start mariadb, connect_cont mariadb)

let close_start mariadb () =
  handle_void (fun () -> B.mysql_close_start mariadb.Common.raw)

let close_cont mariadb status =
  handle_void (fun () -> B.mysql_close_cont mariadb.Common.raw status)

let close mariadb =
  (close_start mariadb, close_cont mariadb)

let fd mariadb =
  Obj.magic @@ B.mysql_get_socket mariadb.Common.raw

let timeout mariadb =
  B.mysql_get_timeout_value mariadb.Common.raw

let timeout_ms mariadb =
  B.mysql_get_timeout_value_ms mariadb.Common.raw

let set_character_set_start mariadb charset () =
  handle_int mariadb ((flip B.mysql_set_character_set_start) charset)

let set_character_set_cont mariadb status =
  handle_int mariadb ((flip B.mysql_set_character_set_cont) status)

let set_character_set mariadb charset =
  (set_character_set_start mariadb charset, set_character_set_cont mariadb)

let select_db_start mariadb db () =
  handle_int mariadb ((flip B.mysql_select_db_start) db)

let select_db_cont mariadb status =
  handle_int mariadb ((flip B.mysql_select_db_cont) status)

let select_db mariadb db =
  (select_db_start mariadb db, select_db_cont mariadb)

let change_user_start mariadb user pass db () =
  handle_char mariadb (fun m -> B.mysql_change_user_start m user pass db)

let change_user_cont mariadb status =
  handle_char mariadb ((flip B.mysql_change_user_cont) status)

let change_user mariadb user pass db =
  (change_user_start mariadb user pass db, change_user_cont mariadb)

let set_server_option_start mariadb opt () =
  let opt = Common.int_of_server_option opt in
  handle_int mariadb ((flip B.mysql_set_server_option_start) opt)

let set_server_option_cont mariadb status =
  handle_int mariadb ((flip B.mysql_set_server_option_cont) status)

let set_server_option mariadb opt =
  (set_server_option_start mariadb opt, set_server_option_cont mariadb)

let ping_start mariadb () =
  handle_int mariadb B.mysql_ping_start

let ping_cont mariadb status =
  handle_int mariadb ((flip B.mysql_ping_cont) status)

let ping mariadb =
  (ping_start mariadb, ping_cont mariadb)

let autocommit_start mariadb auto () =
  handle_char mariadb ((flip B.mysql_autocommit_start) auto)

let autocommit_cont mariadb status =
  handle_char mariadb ((flip B.mysql_autocommit_cont) status)

let autocommit mariadb auto =
  (autocommit_start mariadb auto, autocommit_cont mariadb)

let commit_start mariadb () =
  handle_char mariadb B.mysql_commit_start

let commit_cont mariadb status =
  handle_char mariadb ((flip B.mysql_commit_cont) status)

let commit mariadb =
  (commit_start mariadb, commit_cont mariadb)

let rollback_start mariadb () =
  handle_char mariadb B.mysql_rollback_start

let rollback_cont mariadb status =
  handle_char mariadb ((flip B.mysql_rollback_cont) status)

let rollback mariadb =
  (rollback_start mariadb, rollback_cont mariadb)

let build_stmt mariadb raw =
  match Common.Stmt.init mariadb raw with
  | Some stmt -> `Ok stmt
  | None -> `Error (Common.error mariadb)

let handle_prepare mariadb raw f =
  match f raw with
  | 0, 0 -> build_stmt mariadb raw
  | 0, _ -> `Error (Common.error mariadb)
  | s, _ -> `Wait (Status.of_int s)

let prepare_start mariadb raw_stmt query () =
  handle_prepare mariadb raw_stmt ((flip B.mysql_stmt_prepare_start) query)

let prepare_cont mariadb raw_stmt status =
  handle_prepare mariadb raw_stmt ((flip B.mysql_stmt_prepare_cont) status)

let prepare mariadb query =
  match Common.stmt_init mariadb with
  | Some raw -> `Ok (prepare_start mariadb raw query, prepare_cont mariadb raw)
  | None -> `Error (Common.error mariadb)

module Res = struct
  type t = [`Nonblocking] Common.Res.t

  let num_rows =
    Common.Res.num_rows

  let affected_rows =
    Common.Res.affected_rows

  let handle_fetch (type t) (module R : Row.S with type t = t) res f =
    let stmt = res.Common.Res.stmt in
    match f stmt with
    | 0, 0 -> `Ok (Common.Res.build_row (module R) res)
    | 0, 1 -> `Error (B.mysql_stmt_errno stmt, B.mysql_stmt_error stmt)
    | 0, r when r = T.Return_code.no_data -> `Ok None
    | 0, r when r = T.Return_code.data_truncated ->
        `Error (2032, "truncated data")
    | s, _ -> `Wait (Status.of_int s)

  let fetch_start (type t) (module R : Row.S with type t = t) res () =
    handle_fetch (module R) res B.mysql_stmt_fetch_start

  let fetch_cont (type t) (module R : Row.S with type t = t) res status =
    handle_fetch (module R) res ((flip B.mysql_stmt_fetch_cont) status)

  let fetch (type t) (module R : Row.S with type t = t) res =
    (fetch_start (module R) res, fetch_cont (module R) res)

  let handle_free res f =
    let stmt = res.Common.Res.stmt in
    match f stmt with
    | 0, '\000' -> `Ok ()
    | 0, _ -> `Error (B.mysql_stmt_errno stmt, B.mysql_stmt_error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let free_start res () =
    handle_free res B.mysql_stmt_free_result_start

  let free_cont res status =
    handle_free res ((flip B.mysql_stmt_free_result_cont) status)

  let free res =
    (free_start res, free_cont res)
end

module Stmt = struct
  type t = [`Nonblocking] Common.Stmt.t
  type 'a result = [`Ok of 'a | `Wait of Status.t | `Error of error]

  let init =
    Common.Stmt.init

  let handle_execute stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, 0 -> `Ok stmt
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let execute_start stmt () =
    handle_execute stmt B.mysql_stmt_execute_start

  let execute_cont stmt status =
    handle_execute stmt ((flip B.mysql_stmt_execute_cont) status)

  let execute stmt params =
    let n = B.mysql_stmt_param_count stmt.Common.Stmt.raw in
    let len = Array.length params in
    if n <> len then
      let err = sprintf "parameter count mismatch: %d (expected %d)" len n in
      `Error (2034, err)
    else
      match Common.Stmt.bind_params stmt params with
      | `Ok bound -> `Ok (execute_start bound, execute_cont bound)
      | `Error _ as err -> err

  let handle_store_result stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, 0 -> Common.Stmt.bind_result stmt
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let store_result_start stmt () =
    handle_store_result stmt B.mysql_stmt_store_result_start

  let store_result_cont stmt status =
    handle_store_result stmt ((flip B.mysql_stmt_store_result_cont) status)

  let store_result stmt =
    (store_result_start stmt, store_result_cont stmt)

  let handle_char_unit stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, '\000' -> `Ok ()
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let close_start stmt () =
    handle_char_unit stmt (fun s -> B.mysql_stmt_close_start s)

  let close_cont stmt status =
    handle_char_unit stmt (fun s -> B.mysql_stmt_close_cont s status)

  let close stmt =
    (close_start stmt, close_cont stmt)

  let handle_reset stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, '\000' -> `Ok ()
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let reset_start stmt () =
    handle_reset stmt B.mysql_stmt_reset_start

  let reset_cont stmt status =
    handle_reset stmt ((flip B.mysql_stmt_reset_cont) status)

  let reset mariadb =
    (reset_start mariadb, reset_cont mariadb)

  let handle_next stmt f =
    match f stmt.Common.Stmt.raw with
    | 0, 0 -> `Ok true
    | 0, -1 -> `Ok false
    | 0, _ -> `Error (Common.Stmt.error stmt)
    | s, _ -> `Wait (Status.of_int s)

  let next_result_start stmt =
    handle_next stmt (B.mysql_stmt_next_result_start)

  let next_result_cont stmt status =
    handle_next stmt ((flip B.mysql_stmt_next_result_cont) status)
end

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

  module Time : sig
    type t

    val year : t -> int
    val month : t -> int
    val day : t -> int
    val hour : t -> int
    val minute : t -> int
    val second : t -> int

    val time : hour:int -> minute:int -> second:int -> t
    val local_timestamp : float -> t
    val utc_timestamp : float -> t
    val date : year:int -> month:int -> day:int -> t
    val datetime : year:int -> month:int -> day:int
                -> hour:int -> minute:int -> second:int -> t
  end

  module Field : sig
    type t

    type value =
      [ `Null
      | `Int of int
      | `Float of float
      | `String of string
      | `Bytes of bytes
      | `Time of Time.t
      ]

    val name : t -> string
    val value : t -> value
    val null_value : t -> bool
    val can_be_null : t -> bool

    val int : t -> int
    val float : t -> float
    val string : t -> string
    val bytes : t -> bytes
    val time : t -> Time.t

    val int_opt : t -> int option
    val float_opt : t -> float option
    val string_opt : t -> string option
    val bytes_opt : t -> bytes option
    val time_opt : t -> Time.t option
  end

  module Row : sig
    module type S = sig
      type t
      val build : int -> (int -> Field.t) -> t
    end

    module StringMap : Map.S with type key = string

    module Array : (S with type t = Field.t array)
    module Map : (S with type t = Field.t StringMap.t)
    module Hashtbl : (S with type t = (string, Field.t) Hashtbl.t)
  end

  module Res : sig
    type t

    val num_rows : t -> int
    val affected_rows : t -> int
    val fetch : (module Row.S with type t = 'r) -> t -> 'r option result future
  end

  module Stmt : sig
    type t

    val execute : t -> Field.value array -> Res.t result future
    val reset : t -> unit result future
    val close : t -> unit result future
  end

  type t

  type flag =
    | Compress
    | Found_rows
    | Ignore_sigpipe
    | Ignore_space
    | Interactive
    | Local_files
    | Multi_results
    | Multi_statements
    | No_schema
    | Odbc
    | Ssl
    | Remember_options

  type protocol =
    | Default
    | Tcp
    | Socket
    | Pipe
    | Memory

  type client_option =
    | Connect_timeout of int
    | Compress
    | Named_pipe of string
    | Init_command of string
    | Read_default_file of string
    | Read_default_group of string
    | Set_charset_dir of string
    | Set_charset_name of string
    | Local_infile of bool
    | Protocol of protocol
    | Shared_memory_base_name of string
    | Read_timeout of int
    | Write_timeout of int
    | Secure_auth of bool
    | Report_data_truncation of bool
    | Reconnect of bool
    | Ssl_verify_server_cert of bool
    | Plugin_dir of string
    | Default_auth of string
    | Bind of string
    | Ssl_key of string
    | Ssl_cert of string
    | Ssl_ca of string
    | Ssl_capath of string
    | Ssl_cipher of string
    | Ssl_crl of string
    | Ssl_crlpath of string
    | Connect_attr_reset
    | Connect_attr_add of string * string
    | Connect_attr_delete of string
    | Server_public_key of string
    | Enable_cleartext_plugin of bool

  type server_option =
    | Multi_statements of bool

  val connect : ?host:string
             -> ?user:string
             -> ?pass:string
             -> ?db:string -> ?port:int -> ?socket:string
             -> ?flags:flag list -> unit
             -> t result future

  val close : t -> unit future
  val library_end : unit -> unit
  val set_character_set : t -> string -> unit result future
  val select_db : t -> string -> unit result future
  val change_user : t -> string -> string -> string option -> unit result future
  val set_client_option : t -> client_option -> unit
  val set_server_option : t -> server_option -> unit result future
  val ping : t -> unit result future
  val autocommit : t -> bool -> unit result future
  val commit : t -> unit result future
  val rollback : t -> unit result future
  val prepare : t -> string -> Stmt.t result future
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

  type flag = Common.flag =
    | Compress
    | Found_rows
    | Ignore_sigpipe
    | Ignore_space
    | Interactive
    | Local_files
    | Multi_results
    | Multi_statements
    | No_schema
    | Odbc
    | Ssl
    | Remember_options

  type protocol = Common.protocol =
    | Default
    | Tcp
    | Socket
    | Pipe
    | Memory

  type client_option = Common.client_option =
    | Connect_timeout of int
    | Compress
    | Named_pipe of string
    | Init_command of string
    | Read_default_file of string
    | Read_default_group of string
    | Set_charset_dir of string
    | Set_charset_name of string
    | Local_infile of bool
    | Protocol of protocol
    | Shared_memory_base_name of string
    | Read_timeout of int
    | Write_timeout of int
    | Secure_auth of bool
    | Report_data_truncation of bool
    | Reconnect of bool
    | Ssl_verify_server_cert of bool
    | Plugin_dir of string
    | Default_auth of string
    | Bind of string
    | Ssl_key of string
    | Ssl_cert of string
    | Ssl_ca of string
    | Ssl_capath of string
    | Ssl_cipher of string
    | Ssl_crl of string
    | Ssl_crlpath of string
    | Connect_attr_reset
    | Connect_attr_add of string * string
    | Connect_attr_delete of string
    | Server_public_key of string
    | Enable_cleartext_plugin of bool

  type server_option = Common.server_option =
    | Multi_statements of bool

  let rec nonblocking m (f, g) =
    match f () with
    | `Ok v -> return (Ok v)
    | `Wait s -> W.wait m s >>= fun s -> nonblocking m ((fun () -> g s), g)
    | `Error e -> return (Error e)

  let rec nonblocking' m (f, g) =
    match f () with
    | `Ok -> return_unit
    | `Wait s -> W.wait m s >>= fun s -> nonblocking' m ((fun () -> g s), g)

  module Time = Time
  module Field = Field
  module Row = Row

  module Res = struct
    type t = Res.t

    let fetch (type t) (module R : Row.S with type t = t) res =
      nonblocking res.Common.Res.mariadb (Res.fetch (module R) res)

    let num_rows =
      Res.num_rows

    let affected_rows =
      Res.affected_rows

    let free res =
      nonblocking res.Common.Res.mariadb (Res.free res)
  end

  module Stmt = struct
    type t = Stmt.t

    let handle_execute = function
      | Ok stmt -> nonblocking stmt.Common.Stmt.mariadb (Stmt.store_result stmt)
      | Error _ as e -> return e

    let execute stmt ps =
      match Stmt.execute stmt ps with
      | `Ok nb -> nonblocking stmt.Common.Stmt.mariadb nb >>= handle_execute
      | `Error e -> return (Error e)

    let free_res stmt =
      let handle_free f =
        match f stmt.Common.Stmt.raw with
        | 0, '\000' -> `Ok ()
        | 0, _ -> `Error (Common.Stmt.error stmt)
        | s, _ -> `Wait (Status.of_int s) in
      let start () = handle_free B.mysql_stmt_free_result_start in
      let cont s = handle_free ((flip B.mysql_stmt_free_result_cont) s) in
      nonblocking stmt.Common.Stmt.mariadb (start, cont)

    let reset stmt =
      free_res stmt
      >>= function
      | Ok () -> nonblocking stmt.Common.Stmt.mariadb (Stmt.reset stmt)
      | Error _ as e -> return e

    let close stmt =
      free_res stmt
      >>= function
      | Ok () -> nonblocking stmt.Common.Stmt.mariadb (Stmt.close stmt)
      | Error _ as e -> return e
  end

  let connect ?host ?user ?pass ?db ?(port=0) ?socket ?(flags=[]) () =
    match init () with
    | Some raw ->
        let mariadb = Common.
          { raw
          ; host   = char_ptr_opt_buffer_of_string host
          ; port   = port
          ; user   = char_ptr_opt_buffer_of_string user
          ; pass   = char_ptr_opt_buffer_of_string pass
          ; db     = char_ptr_opt_buffer_of_string db
          ; socket = char_ptr_opt_buffer_of_string socket
          ; flags  = Common.int_of_flags flags
          } in
        nonblocking mariadb (connect mariadb)
    | None ->
        return (Error (2008, "out of memory"))

  let close m = nonblocking' m (close m)

  let library_end = Common.library_end

  let set_character_set m c = nonblocking m (set_character_set m c)

  let select_db m db = nonblocking m (select_db m db)

  let change_user m user pass db = nonblocking m (change_user m user pass db)

  let set_client_option = Common.set_client_option

  let set_server_option m opt = nonblocking m (set_server_option m opt)

  let ping m = nonblocking m (ping m)

  let autocommit m b = nonblocking m (autocommit m b)

  let commit m = nonblocking m (commit m)

  let rollback m = nonblocking m (rollback m)

  let prepare m q =
    match prepare m q with
    | `Ok nb -> nonblocking m nb
    | `Error e -> return (Error e)
end
