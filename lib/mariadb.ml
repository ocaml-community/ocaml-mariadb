module Common = Common
module Blocking = Blocking
module Nonblocking = Nonblocking

type mode = [`Blocking | `Nonblocking]

type error = int * string
