#use "helpers.ml";;

let jolts =
  read_lines "10_input.txt"
  |> List.map int_of_string
  |> List.sort Int.compare;;

let adapter_jolts = L.max jolts + 3;;


(* Part 1 *)
let sum_1_and_3_diff (one, three, prev_jolt) jolt =
  match jolt - prev_jolt with
  | 1 -> (one + 1, three, jolt)
  | 3 -> (one, three + 1, jolt)
  | _ -> (one, three, jolt);;
let jolt_diffs = List.fold_left sum_1_and_3_diff (0, 1, 0) jolts;;
let (diff1, diff3, _) = jolt_diffs in diff1 * diff3;;
(* - : int = 1625 *)


(* Part 2 *)
let range a b = S.range a b |> List.of_seq;;

let arr = let a = 0 :: jolts |> Array.of_list in
  Array.append a [|adapter_jolts|];;
let arr_len = Array.length arr;;

let valid_next i =
  let value = arr.(i) in
  let possible_next = Array.sub arr (i + 1) (min 3 (arr_len - i - 1)) in
  let sum_valid acc n = if n - value <= 3 then acc + 1 else acc in
  Array.fold_left sum_valid 0 possible_next;;

let combination_cache = Array.make arr_len None;;
let rec count_combinations i =
  match combination_cache.(i) with
  | Some n -> n
  | None ->
    if i >= arr_len - 1 then
      (combination_cache.(i) <- Some 1; 1)
    else match valid_next i with
      | 0 -> (combination_cache.(i) <- Some 0; 0)
      | n ->
        let num = (range (i + 1) (i + n + 1))
                  |> List.fold_left (fun acc i -> acc + (count_combinations i)) 0 in
        combination_cache.(i) <- Some num; num;;

count_combinations 0;;
(* - : int = 3100448333024 *)
