open Core
module Generator = Sequence.Generator

(** infinite numbers *)
let numbers_seq () =
  let open Generator in
  let rec f n = yield n >>= fun () -> f (n + 1) in
  f 0 |> Generator.run

(** all ascii lowercase letters *)
let char_seq () = Sequence.map (Sequence.range 97 123) ~f:Char.unsafe_of_int

let named_var_seq () =
  let open Sequence in
  let open Let_syntax in
  let%bind n = numbers_seq () in
  let n' = if n = 0 then "" else Int.to_string n in
  let%bind c = char_seq () in
  String.(concat [ of_char c; n' ]) |> return

let%test_module _ =
  (module struct
    module Generator = Sequence.Generator

    let%test_unit "number generator" =
      let expect = List.range 0 100 in
      let actual =
        numbers_seq () |> Fn.flip Sequence.take 100 |> Sequence.to_list
      in
      [%test_result: int list] actual ~expect

    let%test_unit "char generator" =
      let expect =
        [
          'a';
          'b';
          'c';
          'd';
          'e';
          'f';
          'g';
          'h';
          'i';
          'j';
          'k';
          'l';
          'm';
          'n';
          'o';
          'p';
          'q';
          'r';
          's';
          't';
          'u';
          'v';
          'w';
          'x';
          'y';
          'z';
        ]
      in
      let actual = char_seq () |> Sequence.to_list in
      [%test_result: char list] actual ~expect

    let%test_unit "named variables generator" =
      let expect = [ "a"; "b"; "c"; "d" ] in
      let actual =
        named_var_seq () |> Fn.flip Sequence.take 4 |> Sequence.to_list
      in

      let open Type in
      [%test_result: string list] actual ~expect
  end)
