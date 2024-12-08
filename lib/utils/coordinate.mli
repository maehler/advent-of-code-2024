open! Core

module T : sig
    type t = int * int [@@deriving sexp]
end

include module type of T

val compare : int * int -> int * int -> int
val to_string : int * int -> string
