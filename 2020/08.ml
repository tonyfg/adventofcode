let read_lines filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s |> String.split_on_char '\n';;

let decode instr =
  match Str.string_match (Str.regexp "^\\([a-z]+\\) \\([+-][0-9]+\\)$") instr 0 with
  | true -> Some (Str.matched_group 1 instr, Str.matched_group 2 instr |> int_of_string)
  | false -> None;;

let program = read_lines "08_input.txt" |> List.map decode;;


(* Part 1 *)
module IntSet = Set.Make(Int);;

let rec run ?(cache = IntSet.empty) ?(accum = Ok 0) instrs i =
  let interpret acc i =
    let new_cache = IntSet.add i cache in
    match List.nth instrs i with
    | Some ("acc", value) -> run ~cache:new_cache ~accum:(Ok (acc + value)) instrs (i + 1)
    | Some ("jmp", value) -> run ~cache:new_cache ~accum instrs (i + value)
    | Some (_) -> run ~cache:new_cache ~accum instrs (i + 1)
    | None -> accum in
  match accum, IntSet.find_opt i cache with
  | (Ok accum | Error accum), Some _ -> Error accum
  | Ok accum,    None -> interpret accum i
  | Error accum, None -> interpret accum i;;

run program 0;;
(* - : (int, int) result = Error 1563 *)


(* Part 2 *)
let replace_nth from_instr to_instr n =
  let replace_instr (occur, instrs) instr =
    match instr with
    | None -> (occur, instr :: instrs)
    | Some (ins, value) ->
      if ins = from_instr then
        if occur = n then
          (occur + 1, Some (to_instr, value) :: instrs)
        else
        (occur + 1, instr :: instrs)
      else
        (occur, instr :: instrs) in
  let (_, instrs) = List.fold_left replace_instr (1, []) program in
  instrs |> List.rev;;

let count_instr instr =
  let add_if_match sum ins =
    match ins with
    | None -> sum
    | Some (ins, _) -> if ins = instr then sum + 1 else sum in
  List.fold_left add_if_match 0 program;;

let rec try_until_ok ?(n = 1) from_instr to_instr max =
  if n > max then
    Error 69
  else
    match run (replace_nth from_instr to_instr n) 0 with
    | Ok value -> Ok value
    | Error v -> try_until_ok ~n:(n + 1) from_instr to_instr max;;

match try_until_ok "jmp" "nop" (count_instr "jmp") with
| Ok value -> Ok value
| Error _ -> try_until_ok "nop" "jmp" (count_instr "nop");;
(* - : (int, int) result = Ok 767 *)
