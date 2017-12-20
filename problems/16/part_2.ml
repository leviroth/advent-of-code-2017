open Base
open Stdio

let positions_applied = "gdmhkcieoapbfnlj"
let partners_applied = "hpegikfoanblmcjd"

let ord c = Char.to_int c - Char.to_int 'a'

let compose a b = Map.map a ~f:(Map.find_exn b)

let rec loop transformation current step =
  if step = 0 then current
  else
    let new_transformation = compose transformation transformation in
    if step land 1 = 1 then
      loop new_transformation (compose current transformation) (step lsr 1)
    else loop new_transformation current (step lsr 1)


let () =
  let position_transformation =
    String.to_list positions_applied |> List.mapi ~f:(fun i c -> (i, ord c)) |> Map.of_alist_exn (module Int)
  in
  let partner_transformation =
    String.to_list partners_applied |> List.mapi ~f:(fun i c -> (i, ord c)) |> Map.of_alist_exn (module Int)
  in
  let starting =
    List.zip_exn (List.range 0 16) (List.range 0 16)
    |> Map.of_alist_exn (module Int)
  in
  let position_looped = loop position_transformation starting 1000000000 in
  let partner_looped = loop partner_transformation position_looped 1000000000 in
  Map.to_alist partner_looped |> List.unzip |> snd
  |> List.map ~f:(fun n -> Char.to_int 'a' + n |> Char.of_int_exn)
  |> String.of_char_list |> printf "%s\n"

