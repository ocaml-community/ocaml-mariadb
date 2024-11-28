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
