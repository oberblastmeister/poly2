module Env : sig
  type t

  val empty : t
end

module Error : sig
  type t =
    [ `RecursiveTypes
    | `Unification of Type.t * Type.t
    | `UnboundVariable of Expr.name
    | `UnexpectedNumArgs of int
    | `NotFunction ]
  [@@deriving show, eq]
end

val infer : Env.t -> Type.level -> Expr.t -> (Type.t, [> Error.t ]) result
