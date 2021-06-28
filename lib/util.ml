open Core

(** Throw an exception indicating that something is todo *)
let todo = Error.of_string "Not implemented yet!" |> Error.raise

(** Throw an exception indicating that something should be unreachable *)
let unreachable = Error.of_string "This should be unreachable!" |> Error.raise
