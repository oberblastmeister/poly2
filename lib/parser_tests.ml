open Core

let%test_module "exprs" =
  (module struct
    let test_expr s =
      Parse.parse_expr s |> Expr.sexp_of_t |> Sexp.output_hum Out_channel.stdout

    let%expect_test "lit num" =
      test_expr "1234";
      [%expect {| (Lit (LInt 1234)) |}]

    let%expect_test "add" =
      test_expr "1 + 1";
      [%expect {| (Bin Add (Lit (LInt 1)) (Lit (LInt 1))) |}]

    let%expect_test "sub" =
      test_expr "12 - 13";
      [%expect {| (Bin Sub (Lit (LInt 12)) (Lit (LInt 13))) |}]

    let%expect_test "mul" =
      test_expr "1 * 0";
      [%expect {| (Bin Mul (Lit (LInt 1)) (Lit (LInt 0))) |}]

    let%expect_test "div" =
      test_expr "1 / 132";
      [%expect {| (Bin Div (Lit (LInt 1)) (Lit (LInt 132))) |}]

    let%expect_test "string" =
      test_expr {| "a string" |};
      [%expect {| (Lit (LString "a string")) |}]

    let%expect_test "string with escapes" =
      test_expr {| "\t hello \n \n \" \'" |};
      [%expect
        {|
      (Lit (LString  "\t hello \
                    \n \
                    \n \" '")) |}]

    let%expect_test "precedence correct" =
      test_expr "1 + 3 / 2";
      [%expect
        {|
    (Bin Add (Lit (LInt 1)) (Bin Div (Lit (LInt 3)) (Lit (LInt 2)))) |}]

    let%expect_test "variable" =
      test_expr "a_variable";
      [%expect {| (Var a_variable) |}]

    let%expect_test "let" =
      test_expr {|
    let x = 324 in
    let y = "a string" in
    x + x
    |};
      [%expect
        {|
      (Let x (Lit (LInt 324))
       (Let y (Lit (LString "a string")) (Bin Add (Var x) (Var x)))) |}]

    let%expect_test "simple function" =
      test_expr "fun x -> x";
      [%expect {| (Fun (x) (Var x)) |}]
  end)

let%test_module "types" =
  (module struct
    let test_type s =
      Parse.parse_type s |> Type.sexp_of_t |> Sexp.output_hum Out_channel.stdout

    let%expect_test "t con" =
      test_type "String";
      [%expect {| (Con String) |}]

    let%expect_test "unit" =
      test_type "()";
      [%expect {| Unit |}]

    let%expect_test "arrow type" =
      test_type "(String, Int, Bool) -> String";
      [%expect
        {|
      (Arr ((Con String) (Con Int) (Con Bool)) (Con String)) |}]

    let%expect_test "arrow type unit" =
      test_type "() -> String";
      [%expect {| (Arr (Unit) (Con String)) |}];

      test_type "String -> () -> ()";
      [%expect {| (Arr ((Con String)) (Arr (Unit) Unit)) |}]

    let%expect_test "arrow type assoc correct" =
      test_type "String -> Int -> String -> Bool";
      [%expect
        {|
      (Arr ((Con String)) (Arr ((Con Int)) (Arr ((Con String)) (Con Bool)))) |}]

    let%expect_test "arrow type assoc with parens" =
      test_type "(String -> Int) -> Bool -> (String -> Bool) -> String";
      [%expect
        {|
      (Arr ((Arr ((Con String)) (Con Int)))
       (Arr ((Con Bool)) (Arr ((Arr ((Con String)) (Con Bool))) (Con String)))) |}]
  end)
