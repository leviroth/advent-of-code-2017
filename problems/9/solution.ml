open Base
open Stdio

let rec read_group l depth score garbage_count =
  match l with
  | [] -> (score, garbage_count)
  | '{' :: tl -> read_group tl (depth + 1) score garbage_count
  | '}' :: tl -> read_group tl (depth - 1) (score + depth) garbage_count
  | '<' :: tl -> strip_garbage tl depth score garbage_count
  | _ :: tl -> read_group tl depth score garbage_count

and strip_garbage l depth score garbage_count =
  match l with
  | '!' :: _ :: tl -> strip_garbage tl depth score garbage_count
  | '>' :: tl -> read_group tl depth score garbage_count
  | _ :: tl -> strip_garbage tl depth score (garbage_count + 1)
  | [] -> assert false

let () =
  let s = In_channel.read_all "input.txt" in
  let l = String.to_list s in
  let score, garbage_count = read_group l 0 0 0 in
  printf "Part 1: %d\nPart 2: %d\n" score garbage_count
