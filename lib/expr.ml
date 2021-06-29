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

let is_fun = function Fun _ -> true | _ -> false

module PP = struct
  open Fmt

  let rec pp f = function
    | Var name -> pf f "%s" name
    | Call (expr, args) ->
        if is_fun expr then pf f "(%a) %a" pp expr (list pp) args
        else pf f "%a %a" pp expr (list pp) args
    | Fun (args, body) -> pf f "fun (%a) -> %a" (list string) args pp body
    | Let (name, e1, e2) -> pf f "let %s = %a in %a" name pp e1 pp e2
    | Bin (op, e1, e2) -> pf f "%a %a %a" pp e1 pp_bin_op op pp e2
    | Neg e -> pf f "-%a" pp e
    | Lit l -> pp_lit f l
    | Unit -> pf f "()"

  and pp_bin_op f = function
    | Add -> pf f "+"
    | Sub -> pf f "-"
    | Mul -> pf f "*"
    | Div -> pf f "/"
    | Eq -> pf f "=="
    | NotEq -> pf f "!="

  and pp_lit f = function
    | LInt i -> pf f "%d" i
    | LString s -> pf f "%s" s
    | LBool b -> pf f "%b" b
end

include PP
