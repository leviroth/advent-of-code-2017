open Base
open Stdio

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

let parse s =
  s |> String.rstrip |> String.split ~on:',' |> List.map ~f:Int.of_string


let () =
  let lengths = parse (In_channel.read_all "input.txt") in
  let n = 256 in
  let module M = Rope (struct
    let size = n
  end) in
  let r = M.init () in
  let r' = List.fold ~init:r ~f:M.update lengths in
  printf "%d\n" (M.product r')

