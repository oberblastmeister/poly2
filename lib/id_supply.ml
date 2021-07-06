open Core

type i = int [@@deriving equal, compare, hash, sexp]

type t = i ref [@@deriving sexp]

let create () = ref 0

let next t =
  let id = !t in
  incr t;
  id

let skip t =
  let (_ : int) = next t in
  ()

let skip_n t n =
  for _ = 0 to n do
    skip t
  done
