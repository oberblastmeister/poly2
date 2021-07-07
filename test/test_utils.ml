open Core
open Poly2

let test name = Alcotest.test_case name `Quick

let new_var = (Fn.flip Type.new_var) 1

module TestSetup () = struct
  let supply = Id_supply.create ()

  let a' = new_var supply

  let a = Type.new_gen_var supply

  let b' = new_var supply

  let b = Type.new_gen_var supply

  let c' = new_var supply

  let c = Type.new_gen_var supply

  let d' = new_var supply

  let d = Type.new_gen_var supply

  let e' = new_var supply

  let e = Type.new_gen_var supply
end
