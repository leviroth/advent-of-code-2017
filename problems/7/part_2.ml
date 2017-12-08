open Base
open Stdio

let parse lexbuf = Parser.lines Lexer.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )


let memoize f =
  let table = Hashtbl.Poly.create () in
  fun x ->
    match Hashtbl.find table x with
    | Some y -> y
    | None ->
        let y = f x in
        Hashtbl.add_exn table ~key:x ~data:y ;
        y


type robot =
  {name: string; own_weight: int; total_weight: int; children: robot list}

let rec get_total_weight name name_map =
  let record = String_dict.find_exn name_map name in
  let own_weight = record.Robot_record.weight in
  let other_weights =
    List.fold record.Robot_record.children ~init:0 ~f:(fun total next ->
        total + get_total_weight next name_map )
  in
  own_weight + other_weights


let get_total_weight = memoize get_total_weight

let build_tree name name_map =
  let rec aux name =
    let record = String_dict.find_exn name_map name in
    let children = List.map record.Robot_record.children ~f:aux in
    { name= record.Robot_record.name
    ; own_weight= record.Robot_record.weight
    ; total_weight= get_total_weight name name_map
    ; children }
  in
  aux name


let odd_one_out l key =
  let rec find_length_one = function
    | [] -> None
    | (_, [x]) :: tl -> Some x
    | hd :: tl -> find_length_one tl
  in
  let make_multiset l =
    List.map l ~f:(fun x -> (key x, x)) |> Map.of_alist_multi (module Int)
  in
  find_length_one (Map.to_alist (make_multiset l))


let rec scan_tree t =
  match odd_one_out t.children (fun x -> x.total_weight) with
  | None -> None
  | Some child ->
    match scan_tree child with
    | None ->
        let sibling =
          List.find_exn t.children ~f:(fun x ->
              x.total_weight <> child.total_weight )
        in
        let difference = sibling.total_weight - child.total_weight in
        Some (child.own_weight + difference)
    | Some x -> Some x


let () =
  let robot_records = process_file "input.txt" in
  let name_map =
    String_dict.of_alist_exn
      (List.map robot_records ~f:(fun record -> (record.name, record)))
  in
  let root = build_tree "uownj" name_map in
  scan_tree root |> Option.value_exn |> Stdio.printf "%d\n"

