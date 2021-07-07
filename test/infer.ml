open Core
open Poly2
open Type
open Infer
open Test_utils

let ty_testable = Alcotest.testable Type.pp_sexp Type.equal

let ty_res_testable = Alcotest.testable Infer.Res.pp_sexp Infer.Res.equal

let check_infer ?(name = "") ty expected =
  let actual = infer ty in
  Alcotest.(check ty_res_testable) name expected actual

let check_infer_poly ?(name = "") expr_s expected_s =
  let actual = infer_poly (Parse.parse_expr expr_s) in
  let expected =
    Result.map expected_s ~f:(Fn.compose generalize Parse.parse_type)
  in
  Alcotest.(check ty_res_testable) name expected actual

let check_gen ?(name = "") ty expected =
  let actual = generalize ty in
  Alcotest.(check ty_testable) name expected actual

let function_tests =
  let open TestSetup () in
  [
    test "simple functions" (fun () ->
        check_infer_poly ~name:"id" "fun x -> x" (Ok "a -> a");
        check_infer_poly ~name:"apply" "fun f -> fun x -> f x"
          (Ok "(a -> b) -> a -> b"));
    (* (Fun ([ "f" ], Fun ([ "x" ], Call (Var "f", [ Var "x" ])))) *)
    (* (Fun ([ "f"; "x" ], Call (Var "f", [ Var "x" ]))) *)
    (* (Ok (Arr ([ ]))) ()); *)
  ]

let generalize_tests =
  let open TestSetup () in
  [
    test "simple vars" (fun () ->
        check_gen a' (Var (ref (Generic 0)));
        ());
  ]

let () =
  let open Alcotest in
  run "type inference"
    [ ("functions", function_tests); ("generalization", generalize_tests) ]
