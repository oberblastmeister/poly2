open Core
open Parse

module Expr_tests = struct
  let%expect_test "lit num" =
    parse_expr "1234" |> Expr.print;
    [%expect {| (Expr.Lit (Expr.LInt 1234)) |}]

  let%expect_test "add" =
    parse_expr "1 + 1" |> Expr.print;
    [%expect
      {| (Expr.Bin (Expr.Add, (Expr.Lit (Expr.LInt 1)), (Expr.Lit (Expr.LInt 1)))) |}]

  let%expect_test "sub" =
    parse_expr "12 - 13" |> Expr.print;
    [%expect
      {| (Expr.Bin (Expr.Sub, (Expr.Lit (Expr.LInt 12)), (Expr.Lit (Expr.LInt 13)))) |}]

  let%expect_test "mul" =
    parse_expr "1 * 0" |> Expr.print;
    [%expect
      {| (Expr.Bin (Expr.Mul, (Expr.Lit (Expr.LInt 1)), (Expr.Lit (Expr.LInt 0)))) |}]

  let%expect_test "div" =
    parse_expr "1 / 132" |> Expr.print;
    [%expect
      {| (Expr.Bin (Expr.Div, (Expr.Lit (Expr.LInt 1)), (Expr.Lit (Expr.LInt 132)))) |}]

  let%expect_test "string" =
    parse_expr {| "a string" |} |> Expr.print;
    [%expect {| (Expr.Lit (Expr.LString "a string")) |}]

  let%expect_test "string with escapes" =
    parse_expr {| "\t hello \n \n \" \'" |} |> Expr.print;
    [%expect {| (Expr.Lit (Expr.LString "\t hello \n \n \" '")) |}]

  let%expect_test "precedence correct" =
    parse_expr "1 + 3 / 2" |> Expr.print;
    [%expect
      {|
    (Expr.Bin (Expr.Add, (Expr.Lit (Expr.LInt 1)),
       (Expr.Bin (Expr.Div, (Expr.Lit (Expr.LInt 3)), (Expr.Lit (Expr.LInt 2))))
       )) |}]

  let%expect_test "variable" =
    parse_expr "a_variable" |> Expr.print;
    [%expect {| (Expr.Var "a_variable") |}]

  let%expect_test "let" =
    parse_expr {|
    let x = 324 in
    let y = "a string" in
    x + x
    |}
    |> Expr.print;
    [%expect
      {|
      (Expr.Let ("x", (Expr.Lit (Expr.LInt 324)),
         (Expr.Let ("y", (Expr.Lit (Expr.LString "a string")),
            (Expr.Bin (Expr.Add, (Expr.Var "x"), (Expr.Var "x")))))
         )) |}]
end

module Type_tests = struct
  let%expect_test "t con" =
    parse_type "String" |> Type.print;
    [%expect {| (Type.TCon "String") |}]

  let%expect_test "unit" =
    parse_type "()" |> Type.print;
    [%expect {| Type.TUnit |}]

  let%expect_test "arrow type" =
    parse_type "(String, Int, Bool) -> String" |> Type.print;
    [%expect
      {|
      (Type.TArr ([(Type.TCon "String"); (Type.TCon "Int"); (Type.TCon "Bool")],
         (Type.TCon "String"))) |}]

  let%expect_test "arrow type unit" =
    parse_type "() -> String" |> Type.print;
    [%expect {| (Type.TArr ([Type.TUnit], (Type.TCon "String"))) |}];

    parse_type "String -> () -> ()" |> Type.print;
    [%expect
      {| (Type.TArr ([(Type.TCon "String")], (Type.TArr ([Type.TUnit], Type.TUnit)))) |}]

  let%expect_test "arrow type assoc correct" =
    parse_type "String -> Int -> String -> Bool" |> Type.print;
    [%expect
      {|
      (Type.TArr ([(Type.TCon "String")],
         (Type.TArr ([(Type.TCon "Int")],
            (Type.TArr ([(Type.TCon "String")], (Type.TCon "Bool")))))
         )) |}]

  let%expect_test "arrow type assoc with parens" =
    parse_type "(String -> Int) -> Bool -> (String -> Bool) -> String"
    |> Type.print;
    [%expect
      {|
      (Type.TArr ([(Type.TArr ([(Type.TCon "String")], (Type.TCon "Int")))],
         (Type.TArr ([(Type.TCon "Bool")],
            (Type.TArr ([(Type.TArr ([(Type.TCon "String")], (Type.TCon "Bool")))],
               (Type.TCon "String")))
            ))
         )) |}]
end
