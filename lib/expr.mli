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
[@@deriving equal, compare, sexp]

and lit = LInt of int | LString of string | LBool of bool
[@@deriving compare, sexp]

and bin_op = Add | Sub | Mul | Div | Eq | NotEq
[@@deriving equal, compare, sexp]

val pp : t Fmt.t

val pp_bin_op : bin_op Fmt.t

val pp_lit : lit Fmt.t
