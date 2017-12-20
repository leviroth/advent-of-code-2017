open Base
open Stdio

type 'a node_data = {value: 'a; size: int}

type 'a tree = Leaf | Node of 'a tree * 'a node_data * 'a tree

let size = function Leaf -> 0 | Node (_, {size}, _) -> size

let rec insert_left tree value =
  match tree with
  | Leaf -> Node (Leaf, {value; size= 1}, Leaf)
  | Node (left, d, right) ->
      Node (insert_left left value, {d with size= d.size + 1}, right)


let rec insert_after tree pos value =
  match tree with
  | Leaf -> Node (Leaf, {value; size= 1}, Leaf)
  | Node (left, d, right) ->
      let current_position = size left in
      let new_d = {d with size= d.size + 1} in
      let open Ordering in
      match of_int (Int.compare pos current_position) with
      | Less -> Node (insert_after left pos value, new_d, right)
      | Greater ->
          Node
            (left, new_d, insert_after right (pos - current_position - 1) value)
      | Equal -> Node (left, new_d, insert_left right value)


let rec get tree pos =
  match tree with
  | Leaf -> assert false
  | Node (left, d, right) ->
      let current_position = size left in
      let open Ordering in
      match of_int (Int.compare pos current_position) with
      | Less -> get left pos
      | Greater -> get right (pos - current_position - 1)
      | Equal -> d.value


let compute skip max_step =
  let rec loop tree pos current_step =
    if current_step = max_step then
      let index = (pos + 1) % size tree in
      get tree index
    else
      let newpos = (pos + skip) % size tree in
      loop
        (insert_after tree newpos current_step)
        (newpos + 1) (current_step + 1)
  in
  loop (insert_after Leaf 0 0) 0 1

let () =
  printf "%d\n" (compute 343 2018)
