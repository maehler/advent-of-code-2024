open Sexplib.Std

module T = struct
    type t = int * int [@@deriving sexp]
end

include T

let compare (x1, y1) (x2, y2)  =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | c -> c

let to_string (x, y) =
    "(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ")"
