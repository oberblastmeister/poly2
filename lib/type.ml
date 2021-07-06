open Core

type level = int [@@deriving compare, equal, sexp]

type t = Unit | Con of string | Arr of t list * t | Var of tvar ref
[@@deriving compare, equal, sexp]

and tvar = Unbound of int * level | Link of t | Generic of int
[@@deriving compare, equal, sexp]

let is_arr = function Arr _ -> true | _ -> false

module PP = struct
  open Fmt

  let rec pp' names id_name_map f = function
    | Unit -> pf f "()"
    | Con name -> pf f "%s" name
    | Arr (args, ret) ->
        pf f "%a -> %a"
          (pp_arg_list' names id_name_map)
          args (pp' names id_name_map) ret
    | Var { contents = tv } -> (pp_tvar' names id_name_map) f tv

  and pp_arg_list' names id_name_map f l =
    let pp_surround_if_arr names id_name_map f ty =
      let pp_curried = pp' names id_name_map in
      let pp_v = if is_arr ty then parens pp_curried else pp_curried in
      pf f "%a" pp_v ty
    in

    let rec go f = function
      | [] -> ()
      | hd :: tl ->
          pf f ", %a%a" (pp_surround_if_arr names id_name_map) hd go tl
    in

    match l with
    | [] -> ()
    | [ hd ] -> pf f "%a" (pp_surround_if_arr names id_name_map) hd
    | hd :: tl ->
        pf f "(%a" (pp_surround_if_arr names id_name_map) hd;
        go f tl;
        pf f ")"

  and pp_tvar' names id_name_map f =
    let (module NameSupply : Simplify.NAMESUPPLY) = names in
    let create_or_add = Hashtbl.find_or_add ~default:NameSupply.create in
    function
    | Unbound (id, _) -> pf f "%s" (create_or_add id_name_map id)
    | Link t -> pf f "%a" (pp' names id_name_map) t
    | Generic id -> pf f "%s" (create_or_add id_name_map id)

  let make_base_pp printer =
    printer
      (module Simplify.MakeNameSupply () : Simplify.NAMESUPPLY)
      (Hashtbl.create ~size:4 (module Int))

  let pp f t =
    pp'
      (module Simplify.MakeNameSupply () : Simplify.NAMESUPPLY)
      (Hashtbl.create ~size:4 (module Int))
      f t

  let pp_tvar = make_base_pp pp_tvar'
end

include PP

let string_con = Con "String"

let int_con = Con "Int"

let bool_con = Con "Bool"

let int_op_ty = Arr ([ int_con; int_con ], int_con)

let int_bool_op_ty = Arr ([ int_con; int_con ], bool_con)

let new_var supply level = Var (ref (Unbound (Id_supply.next supply, level)))

let new_gen_var supply = Var (ref (Generic (Id_supply.next supply)))

let%test_module "pretty printing types" =
  (module struct
    let show = Fmt.str "%a" pp

    let new_var = (Fn.flip new_var) 0

    let var id = Var (ref (Unbound (id, 0)))

    let%test_unit "simple type variable incrementing letters" =
      let id_supply = Id_supply.create () in
      [%test_result: t] ~expect:(var 0) (new_var id_supply);
      [%test_result: t] ~expect:(var 1) (new_var id_supply);
      [%test_result: t] ~expect:(var 2) (new_var id_supply);
      [%test_result: t] ~expect:(var 3) (new_var id_supply)

    let%test_unit "it should work after a lot of skipped variables" =
      let id_supply = Id_supply.create () in
      Id_supply.skip_n id_supply 136;
      [%test_result: string] ~expect:"a" (new_var id_supply |> show);
      [%test_result: t] ~expect:(var 138) (new_var id_supply)

    let%test_unit "it should print arrows properly" =
      let id_supply = Id_supply.create () in
      let a = new_var id_supply in
      let b = new_var id_supply in
      [%test_result: string] ~expect:"(a, b) -> a" (Arr ([ a; b ], a) |> show)
  end)
