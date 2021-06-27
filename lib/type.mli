open Core

type level = int [@@deriving show, eq, sexp]

module Unique : sig
  type t

  include Comparable with type t := t
end

type t =
  | Unit
  | TCon of string
  | TApp of t * t list
  | TArr of t list * t
  | TVar of tvar ref
[@@deriving show, eq, sexp]

(* include Equal.S with type t := t *)

and tvar = Unbound of Unique.t * level | Link of t | Generic of Unique.t
[@@deriving show, eq, sexp]

val print : t -> unit

val new_var : level -> t

val new_gen_var : unit -> t
