module Env : sig
  type t

  val empty : unit -> t
end

module Error : sig
  type t =
    [ `RecursiveTypes
    | `UnificationFail of Type.t * Type.t
    | `UnboundVariable of Expr.name
    | `UnexpectedNumArgs of int
    | `NotFunction ]
  [@@deriving show, equal]
end

module Res : sig
  type 'a t' = (Type.t, 'a) result [@@deriving equal, compare]

  type 'a t = (Type.t, ([> Error.t ] as 'a)) result

  val pp : [< Error.t ] t Fmt.t
end

val infer_with : Env.t -> Type.level -> Id_supply.t -> Expr.t -> 'a Res.t

val infer : Expr.t -> 'a Res.t
