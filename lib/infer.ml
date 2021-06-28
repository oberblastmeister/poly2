open Util
open Core

module type ENV = sig
  module StringMap : Map.S

  type t

  val empty : t

  val extend : t -> Expr.name -> Type.t -> t

  val lookup : t -> Expr.name -> Type.t option
end

module Env : ENV = struct
  module StringMap = Map.Make (String)

  type t = Type.t StringMap.t

  let empty = StringMap.empty

  let extend env name ty =
    match StringMap.add env ~key:name ~data:ty with
    | `Ok m -> m
    | `Duplicate -> env

  let lookup env name = StringMap.find env name
end

module Error = struct
  type t =
    [ `RecursiveTypes
    | `Unification of Type.t * Type.t
    | `UnboundVariable of Expr.name ]
  [@@deriving show, eq]
end

let occurs_check_adjust_levels (tvar_id : Type.VarId.t) tvar_level ty =
  let open Result.Let_syntax in
  let rec f = function
    | Type.Var { contents = Type.Link ty } -> f ty
    | Type.Var { contents = Type.Generic _ } -> raise Unreachable
    | Type.Var
        ({ contents = Type.Unbound (other_id, other_level) } as other_tvar) ->
        if Type.VarId.(other_id = tvar_id) then Error `RecursiveTypes
        else if other_level > tvar_level then (
          other_tvar := Type.Unbound (other_id, tvar_level);
          Ok ())
        else Ok ()
    | Type.Arr (param_ty_list, return_ty) ->
        let%bind () = List_ext.iter_result param_ty_list ~f in
        f return_ty
    | Type.Con _ -> Ok ()
    | Type.Unit -> Ok ()
  in

  f ty

let rec unify t1 t2 =
  let open Result.Let_syntax in
  match (t1, t2) with
  | t1, t2 when Type.(equal t1 t2) -> Ok ()
  | Type.Arr (param_ty_list1, return_ty1), Type.Arr (param_ty_list2, return_ty2)
    ->
      let%bind () =
        List_ext.iter_result2 param_ty_list1 param_ty_list2 ~f:unify
      in
      unify return_ty1 return_ty2
  | _ -> Error (`Unification (t1, t2))

let rec generalize level = function
  | Type.Var { contents = Type.Unbound (id, other_level) }
    when other_level > level ->
      Type.Var (ref (Type.Generic id))
  | Type.Arr (param_ty_list, return_ty) ->
      Type.Arr
        ( List.map param_ty_list ~f:(generalize level),
          generalize level return_ty )
  | Type.Var { contents = Type.Link ty } -> generalize level ty
  | ( Type.Var { contents = Type.Generic _ }
    | Type.Var { contents = Type.Unbound _ }
    | Type.Con _ | Type.Unit ) as ty ->
      ty

let instantiate level ty =
  let id_var_map = Hashtbl.create ~size:10 (module Type.VarId) in

  let rec f ty =
    match ty with
    | Type.Var { contents = Type.Link ty } -> f ty
    | Type.Var { contents = Type.Generic id } ->
        Hashtbl.find_or_add id_var_map id ~default:(fun () ->
            Type.new_var level)
    | Type.Var { contents = Type.Unbound _ } -> ty
    | Type.Arr (param_ty_list, return_ty) ->
        Type.Arr (List.map param_ty_list ~f, f return_ty)
    | Type.Con _ -> ty
    | Type.Unit -> ty
  in

  f ty

let rec infer env level =
  let open Result.Let_syntax in
  function
  | Expr.Var name ->
      Env.lookup env name |> Result.of_option ~error:(`UnboundVariable name)
  | Expr.Fun (param_list, body_expr) ->
      let param_ty_list =
        List.map param_list ~f:(fun _ -> Type.new_var level)
      in
      let fn_env =
        List.fold2_exn param_list param_ty_list ~init:env
          ~f:(fun env param_name param_ty -> Env.extend env param_name param_ty)
      in
      let%map return_ty = infer fn_env level body_expr in
      Type.Arr (param_ty_list, return_ty)
  | Expr.Let (var_name, value, body) ->
      let%bind var_ty = infer env (level + 1) value in
      let generalized = generalize level var_ty in
      infer (Env.extend env var_name generalized) level body
