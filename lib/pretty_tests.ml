open Core

let%test_module "types" =
  (module struct
    (* module MakeVarIdTester () = struct *)
    (*   module VarId = Type.MakeVarId () *)

    (*   include VarId *)

    (*   let new_var () = Type.new_var_with (module VarId) 0 *)

    (*   (1* let skip_var n = *1) *)
    (*   (1*   for i = 1 to n do *1) *)
    (*   (1*     let (_ : Type.t) = new_var () in *1) *)
    (*   (1*     () *1) *)
    (*   (1*   done *1) *)
    (* end *)

    (* let pp = Fmt.str "%a" Type.pp *)

    (* let%test_unit "simple type variable" = *)
    (*   let open MakeVarIdTester () in *)
    (*   [%test_result: string] ~expect:"a" (new_var () |> pp) *)

    (* (1* let%test_unit "skip some stuff" = *1) *)
    (* (1* skip_var 4; *1) *)
    (* (1* [%test_result: string] ~expect:"e" (new_var () |> pp) *1) *)

    (* let%test_unit "another test" = *)
    (*   let open MakeVarIdTester () in *)
    (*   [%test_result: string] ~expect:"a" (new_var () |> pp) *)

    (* let%test_unit "another test" = *)
    (*   let open MakeVarIdTester () in *)
    (*   (1* skip_var 2; *1) *)
    (*   [%test_result: string] ~expect:"c" (new_var () |> pp) *)

    (* let%test_unit "another test" = *)
    (*   let open MakeVarIdTester () in *)
    (*   (1* skip_var 4; *1) *)
    (*   [%test_result: string] ~expect:"e" (new_var () |> pp) *)
  end)
