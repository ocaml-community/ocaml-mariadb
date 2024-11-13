This directory contains the test suite.  The main code split up according to
whether the tests are using the blocking or non-blocking API, and for the
latter there are instances depending on the concurrency library:

| Directory           | Description
| ---------           | -----------
| nonblocking         | Abstract tests suite implementation.
| blocking            | Tests the blocking and nonblocking APIs without concurrency.
| nonblocking-async   | Tests the nonblocking API using async.
| nonblocking-lwt     | Tests the nonblocking API using Lwt.

Tests require access to a MariaDB instance to run.  You will likely need to
set some environment variables to point the test suite to the right place:

| Environment variable | Description
| -------------------- | -----------
| `OCAML_MARIADB_HOST` | Host to connect to.
| `OCAML_MARIADB_USER` | Authenticate as the given user.
| `OCAML_MARIADB_PASS` | Authenticate with the given password.
| `OCAML_MARIADB_DB`   | Connect to the given database.
