open Core

type level = int [@@deriving compare, equal, sexp]

include Type_intf

module MakeVarId () : VARID with type t = private Int63.t = struct
  include Unique_id.Int63 ()

  let show = to_string

  let pp f t = Format.pp_print_string f (show t)
end

module VarId = MakeVarId ()

type t = Unit | Con of string | Arr of t list * t | Var of tvar ref
[@@deriving compare, equal, sexp]

and tvar = Unbound of VarId.t * level | Link of t | Generic of VarId.t
[@@deriving compare, equal, sexp]

module PP = struct
  let rec pp' names f =
    let open Fmt in
    function
    | Unit -> pf f "()"
    | Con name -> pf f "%s" name
    | Arr (args, ret) -> pf f "%a -> %a" (list (pp' names)) args (pp' names) ret
    | Var { contents = tv } -> (pp_tvar' names) f tv

  and pp_tvar' names f =
    let open Fmt in
    let (module NameSupply : Simplify.NAMESUPPLY) = names in
    function
    | Unbound _ -> pf f "%s" (NameSupply.create ())
    | Link t -> pf f "%a" (pp' names) t
    | Generic _ -> pf f "%s" (NameSupply.create ())

  let make_base_pp pp' =
    pp' (module Simplify.MakeNameSupply () : Simplify.NAMESUPPLY)

  let pp = make_base_pp pp'

  let pp_tvar = make_base_pp pp_tvar'
end

include PP

let string_con = Con "String"

let int_con = Con "Int"

let bool_con = Con "Bool"

let int_op_ty = Arr ([ int_con; int_con ], int_con)

let int_bool_op_ty = Arr ([ int_con; int_con ], bool_con)

let new_var level = Var (ref (Unbound (VarId.create (), level)))

let new_gen_var () = Var (ref (Generic (VarId.create ())))
