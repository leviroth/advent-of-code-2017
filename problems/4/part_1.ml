open Base
open Stdio

let valid passphrase =
  let words = String.split ~on:' ' passphrase in
  Common.no_duplicates (module String) words

let () =
  Common.print_result valid
