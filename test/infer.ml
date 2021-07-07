open Core
open Poly2
open Type

let infer = Infer.infer

let test name = Alcotest.test_case name `Quick

let ty_testable =
  Alcotest.testable Infer.Res.pp (Infer.Res.equal_t' Infer.Error.equal)

let check_infer ?(name = "") ty expected =
  let actual = infer ty in
  Alcotest.(check ty_testable) name actual expected

let new_var = (Fn.flip new_var) 0

module TestSetup () = struct
  let supply = Id_supply.create ()

  let a = new_var supply

  let b = new_var supply

  let c = new_var supply

  let d = new_var supply

  let e = new_var supply
end

let function_tests =
  let open TestSetup () in
  [
    test "simple functions" (fun () ->
        check_infer (Fun ([ "x" ], Var "x")) (Ok (Arr ([ a ], a)));
        ());
  ]

let () =
  let open Alcotest in
  run "type inference" [ ("functions", function_tests) ]
