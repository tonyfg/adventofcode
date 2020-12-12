#use "helpers.ml";;

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
