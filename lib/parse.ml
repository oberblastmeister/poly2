open Core

type lexer = Lexing.lexbuf -> Parser.token

type 'a parser = lexer -> Lexing.lexbuf -> 'a

let parse p s = p Lexer.token (Lexing.from_string s)

let parse_expr = parse Parser.expr_eof

let parse_type = parse Parser.ty_eof

let replace_ty_constants_with_vars supply { forall = var_name_list; Type.ty } =
  let env =
    List.(var_name_list >>| fun var_name -> (var_name, Type.new_gen_var supply))
    |> Hashtbl.of_alist_exn (module String)
  in

  let rec f ty =
    match ty with
    | Type.Con name -> Hashtbl.find env name |> Option.value ~default:ty
    | Type.Var _ -> ty
    | Type.Arr (param_ty_list, return_ty) ->
        Type.Arr (List.map ~f param_ty_list, f return_ty)
    | Type.Unit -> Type.Unit
  in

  f ty

let parse_forall_type_with s supply =
  parse Parser.ty_forall_eof s |> replace_ty_constants_with_vars supply
