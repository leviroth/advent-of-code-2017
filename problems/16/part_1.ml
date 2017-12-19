open Base
open Stdio

let process_file filename =
  let text = In_channel.read_all filename |> String.rstrip in
  let instruction_strings = String.split ~on:',' text in
  List.map instruction_strings ~f:Instruction.of_string


let () =
  let filename = Caml.Sys.argv.(1) in
  let instructions = process_file filename in
  List.fold instructions ~init:(String.to_array "abcdefghijklmnop") ~f:(fun state instruction -> Instruction.apply instruction state)
  |> Array.to_list |> String.of_char_list |> printf "%s\n"
