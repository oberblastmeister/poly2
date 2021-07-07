open Core
open Poly2
open Type
open Test_utils

let check_pp ?(name = "") ty expected =
  let actual = Fmt.str "%a" pp ty in
  Alcotest.(check string) name expected actual

let variable_tests =
  let open Alcotest in
  [
    test "simple variable" (fun () ->
        let open TestSetup () in
        check_pp (new_var supply) "_a";
        check_pp (new_var supply) "_a";
        check_pp (new_var supply) "_a");
    test "arrows" (fun () ->
        let open TestSetup () in
        check_pp ~name:"it should normalize jumps"
          (Arr ([ a; c ], d))
          "(a, b) -> c";

        check_pp ~name:"it should normalize offset for multi arr"
          (Arr ([ c ], Arr ([ d ], e)))
          "a -> b -> c";

        check_pp ~name:"it should get assoc correct"
          (Arr ([ Arr ([ c ], d) ], a))
          "(a -> b) -> c";

        check_pp ~name:"it should get assoc correct with multiargs"
          (Arr ([ Arr ([ c; c; Arr ([ e ], e) ], d); b; Arr ([ a ], b) ], e))
          "(((a, a, (b -> b)) -> c), d, (e -> d)) -> b");
  ]

let () =
  let open Alcotest in
  run "pretty printing" [ ("variables", variable_tests) ]
