let read_answers filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s |> Str.split (Str.regexp "\n\n")
    |> List.filter (fun s -> not (String.equal s ""));;

let answers = read_answers "06_input.txt";;


(* Part 1 *)
module Cs = Set.Make(Char);;

let count_answers s =
  String.(Cs.(s
    |> split_on_char '\n'
    |> concat ""
    |> String.to_seq
    |> of_seq
    |> cardinal));;

List.fold_left (fun acc s -> acc + count_answers s) 0 answers;;
(* - : int = 6551 *)


(* Part 2 *)
let common_answers s =
  let answers =
    String.split_on_char '\n' s
    |> List.filter (fun s -> not (String.equal s ""))
    |> List.map (fun s -> String.to_seq s |> Cs.of_seq) in
  List.fold_left (fun acc a -> Cs.inter acc a) (List.hd answers) (List.tl answers)
|> Cs.cardinal;;

List.fold_left (fun acc s -> acc + common_answers s) 0 answers;;
(* - : int = 3358 *)
