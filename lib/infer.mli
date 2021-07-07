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
  [@@deriving equal, compare, sexp]

  val pp : t Fmt.t

  val pp_sexp : t Fmt.t

  val show : t -> string
end

module Res : sig
  type 'a t' = (Type.t, [> Error.t ] as 'a) Result.t

  type t = (Type.t, Error.t) Result.t [@@deriving equal, compare, sexp]

  val pp : t Fmt.t

  val pp_sexp : t Fmt.t
end

val infer_with : Env.t -> Type.level -> Id_supply.t -> Expr.t -> 'a Res.t'

val infer : Expr.t -> 'a Res.t'

val infer_poly : Expr.t -> 'a Res.t'

val generalize_with : int -> Type.t -> Type.t

val generalize : Type.t -> Type.t
