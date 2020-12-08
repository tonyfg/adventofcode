let read_lines filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s |> String.split_on_char '\n'
    |> List.filter (fun s -> not (String.equal s ""));;


(* Part 1 *)
let check_slope right down =
  let map = read_lines "03_input.txt"
            |> List.filteri (fun i l -> i mod down = 0) in
  let width = String.length (List.hd map) in
  let has_tree i s = s.[(i * right) mod width] = '#' in
  List.mapi (fun i s -> if has_tree i s then 1 else 0) map
  |> List.fold_left (+) 0;;

check_slope 3 1;;
(* - : int = 237 *)


(* Part 2 *)
List.fold_left
  (fun mult (right, down) -> mult * (check_slope right down))
  1
  [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)];;
(* - : int = 2106818610 *)
