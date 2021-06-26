open Core

type name = string [@@deriving show, eq, sexp]

type t =
  | Var of name
  | Call of t * t list
  | Fun of name list * t
  | Let of name * t * t
[@@deriving show, eq, sexp]
