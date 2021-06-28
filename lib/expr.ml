open Core

type name = string [@@deriving show, equal, compare, sexp]

type t =
  | Var of name
  | Call of t * t list
  | Fun of name list * t
  | Let of name * t * t
  | Bin of bin_op * t * t
  | Neg of t
  | Lit of lit
  | Unit
[@@deriving show, equal, compare, sexp]

and lit = LInt of int | LString of string | LBool of bool
[@@deriving show, compare, sexp]

and bin_op = Add | Sub | Mul | Div | Eq | NotEq [@@deriving show, equal, compare, sexp]

let print t = show t |> print_endline
