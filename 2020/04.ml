#use "helpers.ml";;

let read_passports filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s |> Str.split (Str.regexp "\n\n")
    |> List.map (fun s -> String.map (function | '\n' -> ' ' | c -> c) (String.trim s))
    |> List.filter (fun s -> not (String.equal s ""))
    |> List.map (fun s -> s |> String.split_on_char ' ' |> List.map (String.split_on_char ':'));;


(* Part 1 *)
let passports_with_required_fields =
  let has_field l f = l |> List.(exists (fun entry -> hd entry = f)) in
  let is_valid p =
    ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    |> List.fold_left (fun valid f -> valid && has_field p f) true in
  read_passports "04_input.txt"
  |> List.filter is_valid;;

List.length passports_with_required_fields;;
(* - : int = 182 *)


(* Part 2 *)
(* Validator functions *)
let some l s = l |> List.fold_left (fun acc f -> acc || f s) false;;
let has_length l s = String.length s = l;;
let is_between a b s = s >= a && s <= b;;
let unit_val unit s =
  match matches "\\(^[0-9]+\\)\\([a-z]+\\)$" s with
  | false -> None
  | true -> (match (Str.matched_group 1 s), (Str.matched_group 2 s) with
      | num, u when u = unit -> Some num
      | _ -> None);;
let is_between_unit a b unit s =
  match unit_val unit s with
  | None -> false
  | Some num -> is_between a b num;;
let one_of l s = List.exists (fun a -> a = s) l;;

(* Validation rules for each field *)
let validations = [
  ("byr", [has_length 4; is_between "1920" "2002"]);
  ("iyr", [has_length 4; is_between "2010" "2020"]);
  ("eyr", [has_length 4; is_between "2020" "2030"]);
  ("hgt", [some [
       is_between_unit "150" "193" "cm";
       is_between_unit "59" "76" "in"
    ]]);
  ("hcl", [matches "^#[0-f][0-f][0-f][0-f][0-f][0-f]$"]);
  ("ecl", [one_of ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]]);
  ("pid", [matches "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$"]);
  ("cid", [])
];;

(* Let's put it all together now... *)
let valid_field = function
  | [field; value] ->
    List.assoc field validations
    |> List.for_all (fun validate -> validate value)
  | _ -> false in
let valid_passport p = List.for_all (fun f -> valid_field f) p in
passports_with_required_fields
|> List.fold_left (fun sum p -> if valid_passport p then sum + 1 else sum) 0;;
(* - : int = 109 *)
