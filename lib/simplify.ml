open Core
module Generator = Sequence.Generator

(** infinite numbers *)
let numbers_seq () =
  let open Generator in
  let rec f n = yield n >>= fun () -> f (n + 1) in
  f 0 |> run

(** all ascii lowercase letters *)
let char_seq () = Sequence.range 97 123 |> Sequence.map ~f:Char.unsafe_of_int

let named_var_seq () =
  let open Sequence in
  let open Let_syntax in
  let%bind n = numbers_seq () in
  let n' = if n = 0 then "" else Int.to_string n in
  let%bind c = char_seq () in
  String.(concat [ of_char c; n' ]) |> return

module type NAMESUPPLY = sig
  val create : unit -> string
end

module MakeNameSupply () : NAMESUPPLY = struct
  let seq = ref (named_var_seq ())

  let create () =
    let x, xs = Sequence.next !seq |> Option.value_exn in
    seq := xs;
    x
end

let%test_module _ =
  (module struct
    let letters =
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

    let letters_seq () = Sequence.of_list letters

    let%test_unit "number generator" =
      let expect = List.range 0 100 in
      let actual =
        numbers_seq () |> Fn.flip Sequence.take 100 |> Sequence.to_list
      in
      [%test_result: int list] actual ~expect

    let%test_unit "char generator" =
      let actual = char_seq () |> Sequence.to_list in
      [%test_result: char list] actual ~expect:letters

    let%test_unit "named variables generator" =
      let create (n : int) =
        let open Sequence in
        repeat n
        |> zip (letters_seq ())
        >>| (fun (a, b) -> String.(concat [ of_char a; Int.to_string b ]))
        |> to_list
      in

      let expect =
        List.(
          concat
            [
              letters >>| String.of_char;
              create 1;
              create 2;
              create 3 |> Fn.flip List.take 4;
            ])
      in
      let actual =
        named_var_seq ()
        |> Fn.flip Sequence.take ((26 * 3) + 4)
        |> Sequence.to_list
      in

      [%test_result: string list] actual ~expect
  end)
