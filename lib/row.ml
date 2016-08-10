module type S = sig
  type t
  val build : int -> (int -> Field.t) -> t
end

module Array = struct
  type t = Field.t array
  let build n f = Array.init n f
end

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

module Map = struct
  type t = Field.t StringMap.t

  let build n f =
    let m = ref StringMap.empty in
    for i = 0 to n - 1 do
      let field = f i in
      m := StringMap.add (Field.name field) field !m
    done;
    !m
end

module Hashtbl = struct
  type t = (string, Field.t) Hashtbl.t

  let build n f =
    let h = Hashtbl.create n in
    for i = 0 to n - 1 do
      let field = f i in
      Hashtbl.replace h (Field.name field) field
    done;
    h
end
