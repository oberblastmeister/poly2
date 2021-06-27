open Core

type name = string [@@deriving show, eq, sexp]

type t =
  | Var of name
  | Call of t * t list
  | Fun of name list * t
  | Let of name * t * t
  | Bin of bin_op * t * t
  | Neg of t
  | Lit of lit
[@@deriving show, eq, sexp]

and lit = LInt of int | LString of string [@@deriving show, eq, sexp]

and bin_op = Add | Sub | Mul | Div [@@deriving show, eq, sexp]

let print t = show t |> print_endline
