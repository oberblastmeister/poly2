open Core

type level = int [@@deriving equal, compare, sexp]

type t = Unit | Con of string | Arr of t list * t | Var of tvar ref
[@@deriving compare, equal, sexp]

and tvar = Unbound of int * level | Link of t | Generic of int
[@@deriving compare, equal, sexp]

type forall_naive = { forall : Expr.name list; ty : t }

val pp : t Fmt.t

val pp_sexp : t Fmt.t

val pp_tvar : tvar Fmt.t

val string_con : t

val int_con : t

val bool_con : t

val int_op_ty : t

val int_bool_op_ty : t

val new_var : Id_supply.t -> level -> t

val new_gen_var : Id_supply.t -> t
