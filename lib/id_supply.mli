type i = int [@@deriving equal, compare, hash, sexp]

type t [@@deriving sexp]

val create : unit -> t

val next : t -> int

val skip : t -> unit

val skip_n : t -> int -> unit
