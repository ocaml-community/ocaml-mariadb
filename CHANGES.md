## 1.3.0 - 2025-05-08

  - The `mariadb_config` and `mysql_config` scripts are now used, if
    available, to discover MariaDB client library (#65 by Albert Peschar).
  - Added server-side properties `get_server_info`, `get_server_version`,
    `get_host_info` and `get_proto_info` (#62 by Petter A. Urkedal).
  - Avoid calling `mysql_stmt_free_result` if there is no result set, since
    this is not allowed by recent versions of the client library (by Petter
    A. Urkedal, fixes #64).
  - Avoid possibly blocking calls to `mysql_free_result` in the nonblocking
    implementation.  This was only an issue if a previous result set had not
    been consumed (#68 by Petter A. Urkedal, fixes #67).
  - Fix memory leak in non-blocking test suite (Petter A.  Urkedal, fixes
    #29).

## 1.2.0 - 2024-11-28

  - Added `Stmt.start_txn` (#59 by Corentin Leruth).
  - Added `Res.insert_id` as binding for `mysql_stmt_insert_id` (#58 by
    Corentin Leruth).
  - Updated to support recent OCaml versions (#45 by @kit-ty-kate).
  - Fixed too-early retrieval of statement metadata (#41 by Albert Peschar).
  - Fixed decoding bug for the integer type (#54 by Raman Varabets, tested
    by #61 by Corentin Leruth).
  - Fixed a memory leaks related to result metadata (#39 by Albert Peschar).
  - The build system is now dune and dune-configurator (#52 by Petter A.
    Urkedal) and some of the examples have been converted to a test suite
    (#60 by Petter A. Urkedal).
  - The project has been transferred to ocaml-community with Petter A.
    Urkedal as the new maintainer.
