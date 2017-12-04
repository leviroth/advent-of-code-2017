open Base
open Stdio

module Anagram = struct
  module T = struct
    type t = string [@@deriving sexp_of]
    let compare a b =
      let prepare s = s |> String.to_list |> List.sort ~cmp:[%compare: char] in
      [%compare: char list] (prepare a) (prepare b)
  end
  include T
  include Comparable.Make(T)
end

let valid passphrase =
  let words = String.split ~on:' ' passphrase in
  Common.no_duplicates (module Anagram) words

let () =
  Common.print_result valid
