open Base

module type Size = sig
  val size : int
end

module Loop_array (S : Size) = struct
  type 'a t = 'a array

  let normalize n = n % S.size

  let swap a m n = Array.swap a (normalize m) (normalize n)

  let rev_subarray a start stop =
    let count = (stop - start) / 2 in
    Sequence.iter (Sequence.range 0 count) ~f:(fun n ->
        swap a (start + n) (stop - n - 1) )

end

module Rope (S : Size) = struct
  module L = Loop_array (S)

  type t = {rope: int L.t; position: int; skip: int}

  let init () = {rope= Array.init S.size ~f:Fn.id; position= 0; skip= 0}

  let update rope length =
    L.rev_subarray rope.rope rope.position (rope.position + length) ;
    { rope with
      position= rope.position + length + rope.skip; skip= rope.skip + 1 }

  let product rope = (rope.rope).(0) * (rope.rope).(1)

end

let dense_hash s : int list =
  let input_lengths = s |> String.to_list |> List.map ~f:Char.to_int in
  let lengths = List.append input_lengths [17; 31; 73; 47; 23] in
  let module M = Rope (struct
    let size = 256
  end) in
  let r = ref (M.init ()) in
  for _ = 0 to 63 do r := List.fold ~init:!r ~f:M.update lengths done ;
  let chunks =
    let start_values = List.range ~stride:16 0 256 in
    List.map start_values ~f:(fun start ->
        Array.slice !r.M.rope start (start + 16) )
  in
  List.map chunks ~f:(Array.fold ~init:0 ~f:( lxor ))

let hash_string s : string =
  dense_hash s
  |> List.map ~f:(Printf.sprintf "%02x")
  |> String.concat

