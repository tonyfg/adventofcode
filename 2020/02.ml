type entry = { min: int; max: int; letter: char; password: string };;

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s;;

let regex = Str.regexp "\\(.?+\\)-\\(.?+\\) \\(.?\\): \\(.+\\)";;

let make_entry s =
  let _ = Str.string_match regex s 0 in
  { min = Str.matched_group 1 s |> int_of_string;
    max = Str.matched_group 2 s |> int_of_string;
    letter = (Str.matched_group 3 s).[0];
    password = Str.matched_group 4 s };;

let entries = read_file "02_input.txt"
              |> String.split_on_char '\n'
              |> List.filter (fun s -> not (String.equal s ""))
              |> List.map make_entry;;


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
