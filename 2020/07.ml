#use "helpers.ml";;

let parse s =
  let _ = matches "^\\(.+\\) bags contain \\(.+\\).$" s in
  let container = Str.matched_group 1 s in
  let contained_str = Str.matched_group 2 s in
  let contained_bags s =
    match s with
    | "no other bags" -> None
    | s -> let _ = matches "^ ?\\([0-9]+\\) \\(.+ .+\\) bags?$" s in
      Some ((Str.matched_group 1 s |> int_of_string, Str.matched_group 2 s)) in
  let contained = String.split_on_char ',' contained_str
                  |> List.filter_map contained_bags in
  (container, contained);;


let all_bags =
  read_lines "07_input.txt" |> List.map parse;;


(* Part 1 *)
module StrSet = Set.Make(String);;

let rec get_containers bag =
  let has_my_bag (_, bags) = List.exists (fun (_, b) -> b = bag) bags in
  let my_containers = List.filter has_my_bag all_bags |> List.map (fun (bag, _) -> bag) in
  List.append my_containers (List.map get_containers my_containers |> List.concat);;

get_containers "shiny gold" |> StrSet.of_list |> StrSet.cardinal;;
(* - : int = 337 *)


(* Part 2 *)
let rec count_inner_bags bag =
  match List.assoc_opt bag all_bags with
  | None -> 1
  | Some bags ->
    List.fold_left (fun sum (n, bag) -> sum + (n * (count_inner_bags bag))) 1 bags;;

count_inner_bags "shiny gold" - 1;;
(* - : int = 50100 *)
