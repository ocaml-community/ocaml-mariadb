module type S = sig
  type error = int * string
  type 'a result = ('a, error) Pervasives.result

  type t

  val connect : ?host:string
             -> ?user:string
             -> ?pass:string
             -> ?db:string -> ?port:int -> ?socket:string
             -> unit
             -> t result

  val close : t -> unit
end

module B = Binding_wrappers

module Nonblocking = Nonblocking

let () = B.mysql_library_init 0 None None |> ignore
