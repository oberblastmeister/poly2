open Core

type level = int [@@deriving equal, compare, sexp]

include module type of Type_intf

module VarId : VARID

module MakeVarId () : VARID

type t = Unit | Con of string | Arr of t list * t | Var of tvar ref
[@@deriving compare, equal, sexp]

and tvar = Unbound of VarId.t * level | Link of t | Generic of VarId.t
[@@deriving compare, equal, sexp]

val pp : t Fmt.t

val pp_tvar : tvar Fmt.t

val string_con : t

val int_con : t

val bool_con : t

val int_op_ty : t

val int_bool_op_ty : t

val new_var : level -> t

val new_gen_var : unit -> t
