open Core

let%test_module "types" =
  (module struct
    module MakeTester () = struct
      module Type' = Type.MakeTypes ()

      include Type'

      let show = Fmt.str "%a" Type'.pp

      let new_var () = Type'.new_var 0

      let new_var' () =
        (* this has to be used here for some reason for the unique to register *)
        let s = new_var () |> show in
        let (_ : string) = s in
        ()

      let skip_var n =
        for _ = 1 to n do
          new_var' ()
        done
    end

    let%test_unit "simple type variable incrementing letters" =
      let open MakeTester () in
      [%test_result: string] ~expect:"a" (new_var ());
      [%test_result: string] ~expect:"b" (new_var () |> show);
      [%test_result: string] ~expect:"c" (new_var () |> show);
      [%test_result: string] ~expect:"d" (new_var () |> show)
    (* let v = new_var () in *)
    (* let v = new_var () in *)
    (* [%test_result: string] ~expect:"d" (new_var () |> show) *)

    (* let%test_unit "skip some stuff" = *)
    (*   let open MakeTester () in *)
    (*   skip_var 4; *)
    (*   new_var' (); *)
    (*   new_var' (); *)
    (*   new_var' (); *)
    (*   new_var' (); *)
    (*   [%test_result: string] ~expect:"e" (new_var () |> show) *)

    (* let%test_unit "another test" = *)
    (*   let open MakeTester () in *)
    (*   skip_var 2; *)
    (*   [%test_result: string] ~expect:"c" (new_var () |> show) *)
  end)
