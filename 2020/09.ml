#use "helpers.ml";;

let self_product arr =
  let scalar_product x acc =
    Array.fold_right (fun y acc2 -> Array.append [|[|x; y|]|] acc2) arr acc in
  Array.fold_right scalar_product arr [||];;

let range a b = S.range a b |> List.of_seq;;

let preamble_len = 25;;
let numbers = read_lines "09_input.txt"
              |> List.map int_of_string
              |> Array.of_list;;
let len = Array.length numbers;;

(* Part 1 *)
let check_num i =
  let n = numbers.(i)
  and preamble = Array.sub numbers (i - preamble_len) preamble_len in
  if not (Array.map A.sum (self_product preamble) |> Array.mem n) then
    Some n
  else
    None;;

List.find_map check_num (range preamble_len len);;
(* - : int option = Some 552655238 *)


(* Part 2 *)
let first_invalid_number = 552655238;;

let check_sum i =
  let check_span j =
    let span = Array.sub numbers i (j + 2) in
    let n = span |> A.sum in
    if n >= first_invalid_number then
      Some (n, span)
    else
      None in
  let result = List.find_map check_span (range 0 (len - i - 2)) in
  match result with
  | None -> None
  | Some (n, span) ->
    if n = first_invalid_number then
      Some ((A.min span) + (A.max span))
    else
      None;;

List.find_map check_sum (range 0 len);;
(* - : int option = Some 70672245 *)
