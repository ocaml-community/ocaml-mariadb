module C = Configurator.V1

let preamble ~include_base = Printf.sprintf "#include <%s/mysql.h>" include_base

let detect_src ~include_base =
  Printf.sprintf
    {|
#include <stddef.h>
%s

int
main(void)
{
    MYSQL *m = mysql_init(NULL);
    mysql_close(m);
    return 0;
}
|}
    (preamble ~include_base)

module Variant = struct
  type t = { link_flags : string list; include_base : string }

  let try_compile c { link_flags; include_base } =
    C.c_test c (detect_src ~include_base) ~link_flags
end

let split_flags s = String.split_on_char ' ' (String.trim s)

let use_config cmd c =
  match C.Process.run c cmd [ "--libs" ] with
  | { exit_code = 0; stdout = libs_out; _ } -> (
      match C.Process.run c cmd [ "--variable=pkgincludedir" ] with
      | { exit_code = 0; stdout = pkgincludedir; _ } ->
          Some
            {
              Variant.link_flags = split_flags libs_out;
              include_base = String.trim pkgincludedir;
            }
      | _ -> None)
  | _ -> None

let static v = fun _ -> Some v

let variants =
  Variant.
    [
      use_config "mariadb_config";
      static { link_flags = [ "-lmariadb" ]; include_base = "mariadb" };
      static { link_flags = [ "-lmariadbclient" ]; include_base = "mysql" };
      use_config "mysql_config";
      static { link_flags = [ "-lmysqlclient" ]; include_base = "mysql" };
    ]

let () =
  C.main ~name:"mariadb" @@ fun c ->
  let variant =
    match
      List.find_map
        (fun f ->
          match f c with
          | Some v when Variant.try_compile c v -> Some v
          | _ -> None)
        variants
    with
    | Some v -> v
    | None -> C.die "Cannot find MariaDB client library."
  in
  C.Flags.write_sexp "mariadb_link_flags.sexp" variant.Variant.link_flags;
  C.Flags.write_lines "mariadb_preamble.h"
    [ preamble ~include_base:variant.Variant.include_base ]
