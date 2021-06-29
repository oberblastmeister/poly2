open Core

type level = int [@@deriving show, compare, equal, sexp]

module VarId = struct
  include Unique_id.Int63 ()

  let show = to_string

  let pp f t = Format.pp_print_string f (show t)
end

module Var = struct
  type t =
    | Unbound of VarId.t * level
    | Link of t
    | Generic of VarId.t
    | Named of Expr.name
  [@@deriving show, compare, equal, sexp]
end

type t = Unit | Con of string | Arr of t list * t | Var of tvar ref
[@@deriving show, compare, equal, sexp]

and tvar =
  | Unbound of VarId.t * level
  | Link of t
  | Generic of VarId.t
  | Named of Expr.name
[@@deriving show, compare, equal, sexp]

let string_con = Con "String"

let int_con = Con "Int"

let bool_con = Con "Bool"

let int_op_ty = Arr ([ int_con; int_con ], int_con)

let int_bool_op_ty = Arr ([ int_con; int_con ], bool_con)

let new_var level = Var (ref (Unbound (VarId.create (), level)))

let new_gen_var () = Var (ref (Generic (VarId.create ())))

let print = Fn.compose print_endline show
