open Core

type level = int [@@deriving show, eq, sexp]

module Unique = struct
  include Unique_id.Int63 ()

  let show = to_string

  let pp f t = Format.pp_print_string f (show t)
end

type t =
  | Unit
  | TCon of string
  | TApp of t * t list
  | TArr of t list * t
  | TVar of tvar ref
[@@deriving show, eq, sexp]

and tvar = Unbound of Unique.t * level | Link of t | Generic of Unique.t
[@@deriving show, eq, sexp]

let new_var level = TVar (ref (Unbound (Unique.create (), level)))

let new_gen_var () = TVar (ref (Generic (Unique.create ())))

let print = Fn.compose print_endline show
