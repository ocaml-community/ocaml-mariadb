This directory contains the test suite.  The main code split up according to
whether the tests are using the blocking or non-blocking API, and for the
latter there are instances depending on the concurrency library:

| Directory           | Description
| ---------           | -----------
| blocking            | Tests using the blocking interface.
| nonblocking         | Test library using the non-blocking interface.
| nonblocking-block   | Blocking instance of nonblocking tests.
| nonblocking-async   | Async instance of nonblocking tests.
| nonblocking-lwt     | Lwt instance of nonblocking tests.

Tests require access to a MariaDB instance to run.  You will likely need to
set some environment variables to point the test suite to the right place:

| Environment variable | Description
| -------------------- | -----------
| `OCAML_MARIADB_HOST` | Host to connect to.
| `OCAML_MARIADB_USER` | Authenticate as the given user.
| `OCAML_MARIADB_PASS` | Authenticate with the given password.
| `OCAML_MARIADB_DB`   | Connect to the given database.
