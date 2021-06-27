open Type
open Util
open Core

module type ENV = sig
  module StringMap : Map.S

  type t

  val empty : t

  val extend : t -> Expr.name -> Type.t -> t

  val lookup : t -> Expr.name -> Type.t
end

module Env : ENV = struct
  module StringMap = Map.Make (String)

  type t = Type.t StringMap.t

  let empty = StringMap.empty

  let extend env name ty =
    match StringMap.add env ~key:name ~data:ty with
    | `Ok m -> m
    | `Duplicate -> env

  let lookup env name = StringMap.find_exn env name
end

module Error = struct
  type t = [ `RecursiveTypes ] [@@deriving show, eq]
end

let occurs_check_adjust_levels tvar_id tvar_level ty =
  let open Result.Let_syntax in
  let rec f = function
    | TVar { contents = Link ty } -> f ty
    | TVar { contents = Generic _ } -> raise Unreachable
    | TVar ({ contents = Unbound (other_id, other_level) } as other_tvar) ->
        if Unique.(other_id = tvar_id) then Error `RecursiveTypes
        else if other_level > tvar_level then (
          other_tvar := Unbound (other_id, tvar_level);
          Ok ())
        else Ok ()
    | TApp (ty, ty_arg_list) ->
        let%bind () = List_ext.iter_result ty_arg_list ~f in
        f ty
    | TArr (param_ty_list, return_ty) ->
        let%bind () = List_ext.iter_result param_ty_list ~f in
        f return_ty
    | TCon _ -> Ok ()
    | Unit -> Ok ()
  in

  f ty

let rec unify t1 t2 =
  let open Result.Let_syntax in
  match (t1, t2) with
  | t1, t2 when Type.(equal t1 t2) -> Ok ()
  | TApp (t1, ty_arg_list1), TApp (t2, ty_arg_list2) ->
      let%bind () = unify t1 t2 in
      (* fix the exception here *)
      List_ext.iter_result2 ty_arg_list1 ty_arg_list2 ~f:unify
