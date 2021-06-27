open Core

module Id : sig
  type t [@@deriving show, eq, sexp]

  module type SOURCE = sig
    type s

    val create : s

    val next : s -> t
  end

  module Source : SOURCE
end = struct
  type t = int [@@deriving show, eq, sexp]

  module type SOURCE = sig
    type s

    val create : s

    val next : s -> t
  end

  module Source : SOURCE = struct
    type s = t ref

    let create = ref 0

    let next source =
      let id = !source in
      source := id + 1;
      id
  end
end

type level = int [@@deriving show, eq, sexp]

type t =
  | TCon of string
  | TApp of t * t list
  | TArr of t list * t
  | TVar of tvar
[@@deriving show, eq, sexp]

and tvar = Unbound of Id.t * level | Link of t | Generic of Id.t
[@@deriving show, eq, sexp]

let print t = print_endline (show t)
