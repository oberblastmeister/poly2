open Core
open Poly2
open Type

let new_var = (Fn.flip new_var) 0

let check_pp ?(name = "") ty expected =
  let actual = Fmt.str "%a" pp ty in
  Alcotest.(check string) name expected actual

let test name = Alcotest.test_case name `Quick

module TestSetup () = struct
  let supply = Id_supply.create ()

  let a = new_var supply

  let b = new_var supply

  let c = new_var supply

  let d = new_var supply

  let e = new_var supply
end

let variable_tests =
  let open Alcotest in
  [
    test "simple variable" (fun () ->
        let open TestSetup () in
        check_pp (new_var supply) "a";
        check_pp (new_var supply) "a";
        check_pp (new_var supply) "a");
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
