let read_lines filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s |> String.split_on_char '\n'
    |> List.filter (fun s -> not (s = ""));;

let self_product arr =
  let scalar_product x acc =
    Array.fold_right (fun y acc2 -> Array.append [|[|x; y|]|] acc2) arr acc in
  Array.fold_right scalar_product arr [||];;

let range a b =
  Seq.unfold (fun i -> if i < b then Some(i, i + 1) else None) a
  |> List.of_seq;;

let min arr = Array.fold_left (fun a b -> if a < b then a else b) Int.max_int arr;;
let max arr = Array.fold_left (fun a b -> if a > b then a else b) 0 arr;;
let sum arr = Array.fold_left (+) 0 arr;;

let preamble_len = 25;;
let numbers = read_lines "09_input.txt"
              |> List.map int_of_string
              |> Array.of_list;;
let len = Array.length numbers;;

(* Part 1 *)
let check_num i =
  let n = numbers.(i)
  and preamble = Array.sub numbers (i - preamble_len) preamble_len in
  if not (Array.map sum (self_product preamble) |> Array.mem n) then
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
    let n = span |> sum in
    if n >= first_invalid_number then
      Some (n, span)
    else
      None in
  let result = List.find_map check_span (range 0 (len - i - 2)) in
  match result with
  | None -> None
  | Some (n, span) ->
    if n = first_invalid_number then
      Some ((min span) + (max span))
    else
      None;;

List.find_map check_sum (range 0 len);;
(* - : int option = Some 70672245 *)
