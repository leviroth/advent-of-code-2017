open Base
open Stdio

let parse lexbuf = Parser_2.lines Lexer_2.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )


let enqueue_option queue opt =
  match opt with None -> () | Some x -> Linked_queue.enqueue queue x


let () =
  let open Machine_2 in
  let filename = Caml.Sys.argv.(1) in
  let ast = process_file filename in
  let instructions = Array.of_list ast in
  let state0, state1 = two_programs () in
  let get_instruction state =
    Option.try_with (fun () -> instructions.(state.position))
  in
  let stalled state instruction =
    match instruction with
    | Some Parsetree_2.Rcv _ -> Counter_queue.is_empty state.in_channel
    | Some _ -> false
    | None -> true
  in
  let eval_if_some state instruction =
    match instruction with Some i -> eval state i | None -> state
  in
  let rec loop state0 state1 =
    let instruction0, instruction1 =
      (get_instruction state0, get_instruction state1)
    in
    let deadlocked =
      stalled state0 instruction0 && stalled state1 instruction1
    in
    if deadlocked then !(state1.out_channel.count)
    else
      loop
        (eval_if_some state0 instruction0)
        (eval_if_some state1 instruction1)
  in
  loop state0 state1 |> printf "%d\n"

