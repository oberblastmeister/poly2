open Core
include Type_intf

module MakeTypes () : TYPES = struct
  module VarId = Unique_id.Int63 ()

  type level = int [@@deriving compare, equal, sexp]

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
      | Arr (args, ret) ->
          pf f "%a -> %a" (list (pp' names)) args (pp' names) ret
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
end

include MakeTypes ()

let%test_module "pretty printing types" =
  (module struct
    module MakeTester () = struct
      module Type' = MakeTypes ()

      include Type'

      let show = Fmt.str "%a" Type'.pp

      let new_var () = Type'.new_var 0

      let new_var' () =
        (* this has to be used here for some reason for the unique to register *)
        let s = new_var () |> show in
        let (_ : string) = s in
        ()

      let skip_var n =
        for _ = 1 to n do
          new_var' ()
        done
    end

    let%test_unit "simple type variable incrementing letters" =
      let open MakeTester () in
      [%test_result: string] ~expect:"a" (new_var () |> show);
      [%test_result: string] ~expect:"b" (new_var () |> show);
      [%test_result: string] ~expect:"c" (new_var () |> show);
      [%test_result: string] ~expect:"d" (new_var () |> show)

    let%test_unit "it should work after a lot of skipped variables" =
      let open MakeTester () in
      skip_var 136;
      [%test_result: string] ~expect:"g5" (new_var () |> show)
  end)
