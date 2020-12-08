(*****************
 * SPOILER ALERT *
 *****************)

let read_lines filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s |> String.split_on_char '\n'
    |> List.filter (fun s -> not (String.equal s ""));;

let tickets = read_lines "05_input.txt"


(* Part 1 *)
let rec pow x y = match y with
  | 0 -> 1
  | 1 -> x
  | y -> x * (pow x (y - 1));;

let calc s =
  let offset (i, c) = match c with
    | 'F'|'L' -> 0
    | 'B' -> pow 2 (6 - i)
    | _ -> pow 2 (2 - i) in
  String.to_seqi s |> Seq.map offset |> Seq.fold_left (+) 0;;

let row s = String.sub s 0 7 |> calc;;
let column s = String.sub s 7 3 |> calc;;
let id ticket = (8 * (row ticket)) + (column ticket);;

let select_highest highest ticket =
  let id = id ticket in
  if id > highest then id else highest in
List.fold_left select_highest 0 tickets;;
(* - : int = 926 *)


(* Part 2 *)
let sorted_tickets = List.map id tickets |> List.sort (-);;
let check_ticket (a, b) = match b - a = 1 with
  | false -> Some (a + 1)
  | true -> None in
List.combine sorted_tickets ((List.tl sorted_tickets) @ [1])
|> List.find_map check_ticket;;
(* - : int option = Some 657 *)
