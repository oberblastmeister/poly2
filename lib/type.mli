open Core

type level = int [@@deriving show, equal, compare, sexp]

module VarId : sig
  type t [@@deriving sexp, hash]

  include Comparable.S with type t := t
end

type t = Unit | Con of string | Arr of t list * t | Var of tvar ref
[@@deriving show, compare, equal, sexp]

and tvar =
  | Unbound of VarId.t * level
  | Link of t
  | Generic of VarId.t
  | Named of Expr.name
[@@deriving show, compare, equal, sexp]

val string_con : t

val int_con : t

val bool_con : t

val int_op_ty : t

val int_bool_op_ty : t

val print : t -> unit

val new_var : level -> t

val new_gen_var : unit -> t
