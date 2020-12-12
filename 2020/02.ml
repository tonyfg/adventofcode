#use "helpers.ml";;

type entry = { min: int; max: int; letter: char; password: string };;

let regex = Str.regexp "\\(.?+\\)-\\(.?+\\) \\(.?\\): \\(.+\\)";;

let make_entry s = Str.(
    let _ = string_match regex s 0 in
    { min = matched_group 1 s |> int_of_string;
      max = matched_group 2 s |> int_of_string;
      letter = (matched_group 3 s).[0];
      password = matched_group 4 s });;

let entries = read_lines "02_input.txt" |> List.map make_entry;;


(* Part 1 *)
let add_valid sum entry =
  let count =
    (String.split_on_char entry.letter entry.password |> List.length) - 1 in
  if count >= entry.min && count <= entry.max then
    sum + 1
  else
    sum in
List.fold_left add_valid 0 entries;;
(* - : int = 515 *)


(* Part 2 *)
let add_valid sum entry =
  if (entry.password.[entry.min - 1] = entry.letter) <>
     (entry.password.[entry.max - 1] = entry.letter) then
    sum + 1
  else
    sum in
List.fold_left add_valid 0 entries;;
(* - : int = 711 *)
