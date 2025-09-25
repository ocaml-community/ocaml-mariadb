open Printf

module type IO = sig
  type 'a future
  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
  val return : 'a -> 'a future
end

module Make
    (IO : IO)
    (M : Mariadb.Nonblocking.S with type 'a future := 'a IO.future) =
struct
  open IO

  let (>|=) m f = m >>= fun x -> return (f x)

  let env var def = try Sys.getenv var with Not_found -> def

  let die_f ppf = ksprintf (fun msg -> eprintf "%s\n%!" msg; exit 2) ppf

  let or_die where = function
    | Ok r -> return r
    | Error (i, e) -> eprintf "%s: (%d) %s\n%!" where i e; exit 2

  let rec iter_s_list f = function
    | [] -> return ()
    | x :: xs -> f x >>= fun () -> iter_s_list f xs

  let rec map_s_list f = function
    | [] -> return []
    | x :: xs -> f x >>= fun y -> map_s_list f xs >|= fun ys -> y :: ys

  let connect () =
    M.connect
      ~host:(env "OCAML_MARIADB_HOST" "localhost")
      ~user:(env "OCAML_MARIADB_USER" "root")
      ~pass:(env "OCAML_MARIADB_PASS" "")
      ~db:(env "OCAML_MARIADB_DB" "mysql")
      ~port:(int_of_string (env "OCAML_MARIADB_PORT" "0")) ()

  let rec repeat n f =
    if n = 0 then return () else f () >>= fun () -> repeat (n - 1) f

  let string_of_param_type = function
    | `Int -> "integer"
    | `Float -> "double"
    | `String | `Bytes -> "char"
    | `Time -> "datetime"

  let random_string () =
    let n = Random.int (1 lsl Random.int 8) in
    String.init n (fun _ -> "ACGT".[Random.int 4])

  let random_param_type _ =
    match Random.int 5 with
    | 0 -> `Int
    | 1 -> `Float
    | 2 -> `String
    | 3 -> `Bytes
    | 4 -> `Time
    | _ -> assert false

  let random_param param_type =
    if Random.int 6 = 0 then `Null else
    match param_type with
    | `Int -> `Int (Random.bits ())
    | `Float -> `Float (ldexp (Random.float 2.0 -. 1.0) (Random.int 16))
    | `String -> `String (random_string ())
    | `Bytes -> `Bytes (Bytes.of_string (random_string ()))
    | `Time -> `Time (M.Time.utc_timestamp (Random.float 1577833200.0))

  let make_nary_select_stmt dbh param_types =
    let buf = Buffer.create 64 in
    Buffer.add_string buf "SELECT ";
    for i = 0 to Array.length param_types - 1 do
      if i > 0 then Buffer.add_string buf ", ";
      bprintf buf "CAST(? AS %s)" (string_of_param_type param_types.(i))
      (* CAST is only used as a type annotation to prevent the parameters from
       * being cast. *)
    done;
    M.prepare dbh (Buffer.contents buf) >>= or_die "prepare"

  let string_of_timestamp t =
    let y, mon, day = M.Time.(year t, month t, day t) in
    let h, m, s, us = M.Time.(hour t, minute t, second t, microsecond t) in
    sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%06d" y mon day h m s us

  let string_of_value = function
    | `Null -> "NULL"
    | `Int i -> sprintf "(%d : int)" i
    | `Int64 i -> sprintf "(%Ld : int64)" i
    | `UInt64 i -> sprintf "(%s : uint64)" (Unsigned.UInt64.to_string i)
    | `Float x -> sprintf "(%.8g : float)" x
    | `String s -> sprintf "(%S : string)" s
    | `Bytes s -> sprintf "(%S : bytes)" (Bytes.to_string s)
    | `Time t -> string_of_timestamp t

  let equal_float x x' =
    abs_float (x -. x') /. (abs_float (x +. x') +. epsilon_float) < 1e-6

  let equal_time t t' =
    let open M.Time in let open Stdlib in
    (* Treat `Datetime and `Timestamp as equal. *)
    year t = year t' && month t = month t' && day t = day t' &&
    hour t = hour t' && minute t = minute t' && second t = second t'

  let equal_field v v' =
    match v, v' with
    | `Null, `Null -> true
    | `Null, _ | _, `Null -> false
    | `Int i, `Int i' -> i = i'
    | `Int i, `Float x | `Float x, `Int i -> float_of_int i = x
    | `Int64 i, `Int64 i' -> Int64.equal i i'
    | `Int64 i, `Int x | `Int x, `Int64 i -> Int64.(equal i (of_int x))
    | `Int64 i, `Float x | `Float x, `Int64 i -> Int64.to_float i = x
    | `UInt64 i, `UInt64 i' -> Unsigned.UInt64.equal i i'
    | `UInt64 i, `Int x | `Int x, `UInt64 i -> Unsigned.UInt64.(equal i (of_int x))
    | `UInt64 i, `Float x | `Float x, `UInt64 i -> Int64.to_float (Unsigned.UInt64.to_int64 i) = x
    | `UInt64 i, `Int64 x | `Int64 x, `UInt64 i -> Int64.equal (Unsigned.UInt64.to_int64 i) x
    | `Int _, _ | _, `Int _ -> false
    | `Int64 _, _ | _, `Int64 _ -> false
    | `UInt64 _, _ | _, `UInt64 _ -> false
    | `Float x, `Float x' -> equal_float x x'
    | `Float _, _ | _, `Float _ -> false
    | `String s, `String s' -> s = s'
    | `String s, `Bytes s' | `Bytes s', `String s -> s = Bytes.to_string s'
    | `String _, _ | _, `String _ -> false
    | `Bytes s, `Bytes s' -> s = s'
    | `Bytes _, _ | _, `Bytes _ -> false
    | `Time t, `Time t' -> equal_time t t'

  let assert_field_equal v v' =
    if not (equal_field v v') then begin
      eprintf "Parameter %s came back as %s.\n%!"
              (string_of_value v) (string_of_value v');
      exit 2
    end

  let execute_no_data stmt =
    M.Stmt.execute stmt [||] >>= or_die "execute" >|= fun res ->
    assert (M.Res.num_rows res = 0)

  let fetch_single_row res =
    assert (M.Res.num_rows res = 1);
    M.Res.fetch (module M.Row.Array) res >>= or_die "fetch" >|= fun row ->
    (match row with
     | None -> failwith "expecting one row, no rows returned"
     | Some a -> a)

  let test_server_properties () =
    connect () >>= or_die "connect" >>= fun dbh ->
    let v = M.get_server_version dbh in
    assert (v >= 10000 && v < 10000000); (* 1 <= major_version < 1000 *)
    let info = M.get_server_info dbh in
    let info' = sprintf "%d.%d.%d" (v / 10000) (v / 100 mod 100) (v mod 100) in
    assert (List.hd (String.split_on_char '-' info) = info');
    let host = M.get_host_info dbh in
    assert (String.length host < 1024);
    for i = 0 to String.length host - 1 do
      match host.[i] with
      | '\x20'..'\x7f' -> ()
      | _ -> die_f "result from get_host_info looks suspicious: %S" host
    done;
    let proto = M.get_proto_info dbh in
    assert (proto >= 0 && proto < 10000); (* it's 10 for MariaDB 10.11.8 *)
    return ()

  let test_insert_id () =
    connect () >>= or_die "connect" >>= fun dbh ->
    M.prepare dbh
      "CREATE TEMPORARY TABLE ocaml_mariadb_test \
        (id integer PRIMARY KEY AUTO_INCREMENT)"
      >>= or_die "prepare"
      >>= fun create_table_stmt ->
    execute_no_data create_table_stmt >>= fun () ->
    M.prepare dbh "INSERT INTO ocaml_mariadb_test VALUES (DEFAULT)"
      >>= or_die "prepare"
      >>= fun insert_stmt ->
    let rec check_inserts_from expected_id =
      if expected_id > 5 then return () else
      M.Stmt.execute insert_stmt [||] >>= or_die "insert" >>= fun res ->
      assert (M.Res.num_rows res = 0);
      assert (M.Res.insert_id res = expected_id);
      check_inserts_from (expected_id + 1)
    in
    check_inserts_from 1 >>= fun () ->
    M.close dbh

  let test_txn () =
    connect () >>= or_die "connect" >>= fun dbh ->

    M.prepare dbh
      "CREATE TEMPORARY TABLE ocaml_mariadb_test (i integer PRIMARY KEY)"
      >>= or_die "prepare create_table_stmt"
      >>= fun create_table_stmt ->
    execute_no_data create_table_stmt >>= fun () ->

    map_s_list (fun s -> M.prepare dbh s >>= or_die "prepare")
      ["INSERT INTO ocaml_mariadb_test VALUES (1), (2)";
       "INSERT INTO ocaml_mariadb_test SELECT i + 10 FROM ocaml_mariadb_test"]
      >>= fun insert_stmts ->
    M.prepare dbh "SELECT CAST(sum(i) AS integer) FROM ocaml_mariadb_test"
      >>= or_die "prepare sum"
      >>= fun sum_stmt ->

    M.start_txn dbh >>= or_die "start_txn" >>= fun () ->
    iter_s_list execute_no_data insert_stmts >>= fun () ->
    M.rollback dbh >>= or_die "rollback" >>= fun () ->
    M.Stmt.execute sum_stmt [||] >>= or_die "execute" >>= fun res ->
    fetch_single_row res >>= fun row ->
    assert (Array.length row = 1 && M.Field.null_value row.(0));

    M.start_txn dbh >>= or_die "start_txn" >>= fun () ->
    iter_s_list execute_no_data insert_stmts >>= fun () ->
    M.commit dbh >>= or_die "rollback" >>= fun () ->
    M.Stmt.execute sum_stmt [||] >>= or_die "execute" >>= fun res ->
    fetch_single_row res >>= fun row ->
    assert (Array.length row = 1 && M.Field.int row.(0) = 26);

    M.close dbh

  (* Make sure the conversion between timestamps and strings are consistent
   * between MariaDB and OCaml. By sending timestamps to be compared as binary
   * and as string, this also verifies the MYSQL_TIME encoding. *)
  let test_datetime_and_string_conv dbh =
    let t = M.Time.utc_timestamp (Random.float 1577833200.0) in
    let s = string_of_timestamp t in
    M.prepare dbh "SELECT CAST(? AS DATETIME), DATE_FORMAT(?, '%Y-%m-%dT%T.%f')"
      >>= or_die "prepare" >>= fun stmt ->
    let params = [|`String s; `Time t|] in
    M.Stmt.execute stmt params >>= or_die "Stmt.execute" >>= fun res ->
    assert (M.Res.num_rows res = 1);
    M.Res.fetch (module M.Row.Array) res >>= or_die "Res.fetch" >>= fun row ->
    M.Stmt.close stmt
      >>= or_die "Stmt.close in test_datetime_and_string_conv"
      >|= fun () ->
    (match row with
     | Some [|t'; s'|] ->
        assert (equal_time t M.Field.(time t'));
        assert (s = M.Field.(string s'))
     | _ -> assert false)

  let test_random_select () =
    let stmt_cache = Hashtbl.create 7 in
    connect () >>= or_die "connect" >>= fun dbh ->
    test_datetime_and_string_conv dbh >>= fun () ->
    repeat 100 begin fun () ->
      let n = Random.int (1 lsl Random.int 8) + 1 in
      let param_types = Array.init n random_param_type in
      let params = Array.map random_param param_types in
      begin
        try
          return (Hashtbl.find stmt_cache param_types)
        with Not_found ->
          make_nary_select_stmt dbh param_types
      end >>= fun stmt ->
      M.Stmt.execute stmt params >>= or_die "Stmt.execute" >>= fun res ->
      assert (M.Res.num_rows res = 1);
      M.Res.fetch (module M.Row.Array) res >>= or_die "Res.fetch" >>=
      (function
       | None -> assert false
       | Some row ->
          assert (Array.length row = Array.length params);
          for i = 0 to n - 1 do
            assert_field_equal params.(i) (M.Field.value row.(i))
          done;
          return ()) >>= fun () ->
      if Random.bool () then
        M.Stmt.close stmt >>= or_die "Stmt.close" >|= fun () ->
        Hashtbl.remove stmt_cache param_types
      else
        M.Stmt.reset stmt >>= or_die "Stmt.reset" >|= fun () ->
        Hashtbl.replace stmt_cache param_types stmt
    end >>= fun () ->
    Hashtbl.fold
      (fun _ stmt prologue ->
        prologue >>= fun () ->
        M.Stmt.close stmt >>= or_die "Stmt.close")
      stmt_cache (return ()) >>= fun () ->
    M.close dbh

  let test_many_select () = repeat 500 test_random_select

  let test_integer, test_bigint =
    let make_check type_ =
      connect () >>= or_die "connect" >>= fun dbh ->
      M.prepare dbh
        (Printf.sprintf
           "CREATE TEMPORARY TABLE ocaml_mariadb_test (id integer PRIMARY KEY \
            AUTO_INCREMENT, value %s, value_unsigned %s unsigned)"
           type_ type_)
      >>= or_die "prepare create"
      >>= fun create_table_stmt ->
      execute_no_data create_table_stmt >>= fun () ->
      let check (value : [ `Signed of int | `Unsigned of int ]) =
        let column =
          match value with
          | `Signed _ -> "value"
          | `Unsigned _ -> "value_unsigned"
        in
        M.prepare dbh
          (Printf.sprintf "INSERT INTO ocaml_mariadb_test (%s) VALUES (?)"
             column)
        >>= or_die "prepare insert"
        >>= fun insert_stmt ->
        let value_to_insert =
          match value with `Signed n -> n | `Unsigned n -> n
        in
        M.Stmt.execute insert_stmt [| `Int value_to_insert |]
        >>= or_die "insert"
        >>= fun res ->
        M.prepare dbh
          (Printf.sprintf "SELECT %s FROM ocaml_mariadb_test WHERE id = (?)"
             column)
        >>= or_die "prepare select"
        >>= fun select_stmt ->
        M.Stmt.execute select_stmt [| `Int (M.Res.insert_id res) |]
        >>= or_die "Stmt.execute"
        >>= M.Res.fetch (module M.Row.Array)
        >>= or_die "Res.fetch"
        >|= function
        | Some [| inserted_value |] ->
            assert_field_equal (`Int value_to_insert)
              (`Int (M.Field.int inserted_value))
        | _ -> assert false
      in
      return (dbh, check)
    in
    let test_integer () =
      make_check "integer" >>= fun (dbh, check) ->
      let input =
        [
          `Signed
            (Int32.max_int |> Int32.to_int (* max value for integer column *));
          `Signed
            (Int32.min_int |> Int32.to_int (* min value for integer column *));
          `Unsigned (Unsigned.UInt32.max_int |> Unsigned.UInt32.to_int)
          (* max value for unsgined integer column.
             Produces the following error: insert: (1264) Out of range value for column 'value_unsigned' at row 1 *);
        ]
      in
      iter_s_list check input >>= fun () -> M.close dbh
    in
    let test_bigint () =
      make_check "bigint" >>= fun (dbh, check) ->
      let input =
        [
          `Signed max_int
          (* [max_int] is below the max value for bigint column (which is equivalent to [Int64.max_int])
             Produces the following error: Parameter (4611686018427387903 : int) came back as (-1 : int) *);
          `Unsigned max_int
          (* insert: (1264) Out of range value for column 'value_unsigned' at row 1 *);
        ]
      in
      iter_s_list check input >>= fun () -> M.close dbh
    in
    (test_integer, test_bigint)

  let test_json () =
    connect () >>= or_die "connect" >>= fun dbh ->

    (* Create a test table with JSON column *)
    M.prepare dbh
      "CREATE TEMPORARY TABLE ocaml_mariadb_json_test (id integer PRIMARY KEY AUTO_INCREMENT, data JSON)"
      >>= or_die "prepare create json table"
      >>= fun create_table_stmt ->
    execute_no_data create_table_stmt >>= fun () ->

    (* Test inserting JSON data *)
    M.prepare dbh "INSERT INTO ocaml_mariadb_json_test (data) VALUES (?)"
      >>= or_die "prepare insert json"
      >>= fun insert_stmt ->

    (* Test various JSON types *)
    let test_cases = [
      {|{"name": "John", "age": 30}|};
      {|[1, 2, 3, "four"]|};
      {|"simple string"|};
      {|42|};
      {|true|};
      {|null|}
    ] in

    (* Insert all test cases *)
    iter_s_list (fun json_data ->
      M.Stmt.execute insert_stmt [| `String json_data |] >>= or_die "insert json"
      >|= fun _ -> ()
    ) test_cases >>= fun () ->

    (* Select and verify we can retrieve JSON data *)
    M.prepare dbh "SELECT id, data FROM ocaml_mariadb_json_test ORDER BY id"
      >>= or_die "prepare select json"
      >>= fun select_stmt ->
    M.Stmt.execute select_stmt [||] >>= or_die "execute select json" >>= fun res ->

    (* Verify we can fetch and access JSON fields *)
    let rec verify_rows count =
      M.Res.fetch (module M.Row.Array) res >>= or_die "fetch json row" >>= function
      | Some row ->
          assert (Array.length row = 2);
          (* Test that we can access the JSON field using different methods *)
          let json_value = match M.Field.value row.(1) with
            | `Bytes b -> Bytes.to_string b
            | _ -> failwith "Expected JSON field as Bytes"
          in
          (* Verify we got some data back *)
          assert (String.length json_value > 0);

          (* Test accessor functions *)
          let json_direct = M.Field.bytes row.(1) in
          let json_opt = M.Field.bytes_opt row.(1) in
          assert (json_opt = Some json_direct);
          assert (Bytes.length json_direct > 0);

          verify_rows (count + 1)
      | None ->
          (* We should have retrieved all our test cases *)
          assert (count = List.length test_cases);
          return ()
    in

    verify_rows 0 >>= fun () ->

    (* Test JSON functions if supported (optional) *)
    (try
      M.prepare dbh "SELECT JSON_TYPE(data) FROM ocaml_mariadb_json_test LIMIT 1"
        >>= or_die "prepare json type"
        >>= fun json_func_stmt ->
      M.Stmt.execute json_func_stmt [||] >>= or_die "execute json type" >>= fun res ->
      M.Res.fetch (module M.Row.Array) res >>= or_die "fetch json type" >>= function
      | Some row ->
          let json_type = match M.Field.value row.(0) with
            | `String s -> s
            | _ -> failwith "Expected String from JSON_TYPE"
          in
          (* JSON_TYPE should return something like "OBJECT", "ARRAY", etc. *)
          assert (String.length json_type > 0);
          M.Stmt.close json_func_stmt >>= or_die "close json func stmt"
      | None -> return ()
    with
    | _ -> return () (* JSON functions might not be supported in all versions *)
    ) >>= fun () ->

    M.Stmt.close select_stmt >>= or_die "close select stmt" >>= fun () ->
    M.Stmt.close insert_stmt >>= or_die "close insert stmt" >>= fun () ->
    M.close dbh

  let main () =
    test_server_properties () >>= fun () ->
    test_insert_id () >>= fun () ->
    test_txn () >>= fun () ->
    test_json () >>= fun () ->
    test_many_select () >>= fun () ->
    test_integer () >>= fun () -> test_bigint ()
end
