open Core
include List

let iter_result list ~f = List.fold_result list ~init:() ~f:(fun () a -> f a)

let iter_result2 list1 list2 ~f =
  iter_result (List.zip_exn list1 list2) ~f:(fun (a, b) -> f a b)
