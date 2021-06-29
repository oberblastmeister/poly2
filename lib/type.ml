open Core

type level = int [@@deriving compare, equal, sexp]

module VarId = struct
  include Unique_id.Int63 ()

  let show = to_string

  let pp f t = Format.pp_print_string f (show t)
end

type t = Unit | Con of string | Arr of t list * t | Var of tvar ref
[@@deriving compare, equal, sexp]

and tvar = Unbound of VarId.t * level | Link of t | Generic of VarId.t
[@@deriving compare, equal, sexp]

module PP = struct
  open Fmt

  let rec pp f =
    let open Fmt in
    function
    | Unit -> pf f "()"
    | Con name -> pf f "%s" name
    | Arr (args, ret) -> pf f "%a -> %a" (list pp) args pp ret
    | Var { contents = tv } -> pp_tvar f tv

  and pp_tvar f =
    let module NameSupply = Simplify.MakeNameSupply () in
    let open Fmt in
    function
    | Unbound _ -> pf f "%s" (NameSupply.create ())
    | Link t -> pf f "%a" pp t
    | Generic _ -> pf f "%s" (NameSupply.create ())
end

include PP

let string_con = Con "String"

let int_con = Con "Int"

let bool_con = Con "Bool"

let int_op_ty = Arr ([ int_con; int_con ], int_con)

let int_bool_op_ty = Arr ([ int_con; int_con ], bool_con)

let new_var level = Var (ref (Unbound (VarId.create (), level)))

let new_gen_var () = Var (ref (Generic (VarId.create ())))
