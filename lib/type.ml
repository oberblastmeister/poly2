open Core

type level = int [@@deriving show, eq, sexp]

module VarId = struct
  include Unique_id.Int63 ()

  let show = to_string

  let pp f t = Format.pp_print_string f (show t)
end

type t =
  | Unit
  | Con of string
  | Arr of t list * t
  | Var of tvar ref
[@@deriving show, eq, sexp]

and tvar = Unbound of VarId.t * level | Link of t | Generic of VarId.t
[@@deriving show, eq, sexp]

let new_var level = Var (ref (Unbound (VarId.create (), level)))

let new_gen_var () = Var (ref (Generic (VarId.create ())))

let print = Fn.compose print_endline show
