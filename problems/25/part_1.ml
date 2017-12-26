open Base
open Stdio

let () =
  Fn.apply_n_times ~n:12586542 Machine.step Machine.starter
  |> Machine.checksum
  |> printf "%d\n"
