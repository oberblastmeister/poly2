open Core

type level = int [@@deriving show, eq, sexp]

module VarId : sig
  type t [@@deriving sexp, hash]

  include Comparable.S with type t := t
end

type t = Unit | Con of string | Arr of t list * t | Var of tvar ref
[@@deriving show, eq, sexp]

(* include Equal.S with type t := t *)
and tvar = Unbound of VarId.t * level | Link of t | Generic of VarId.t
[@@deriving show, eq, sexp]

val print : t -> unit

val new_var : level -> t

val new_gen_var : unit -> t
