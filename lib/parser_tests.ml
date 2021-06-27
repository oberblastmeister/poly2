open Core
open Parse

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

let%expect_test "precedence correct" =
  parse_expr "1 + 3 / 2" |> Expr.print;
  [%expect
    {|
    (Expr.Bin (Expr.Add, (Expr.Lit (Expr.LInt 1)),
       (Expr.Bin (Expr.Div, (Expr.Lit (Expr.LInt 3)), (Expr.Lit (Expr.LInt 2))))
       )) |}]
