#use "helpers.ml";;

let numbers = read_lines "01_input.txt" |> List.map int_of_string;;


(* Part 1 *)
List.find_map (fun m ->
    List.find_map (fun n ->
        match m + n with
        | 2020 -> Some(m * n)
        | _ -> None
      ) numbers
  ) numbers;;
(* - : int = 1016619 *)


(* Part 2 *)
List.find_map (fun m ->
    List.find_map (fun n ->
        List.find_map (fun o ->
            match n + m + o with
            | 2020 -> Some(n * m * o)
            | _ -> None
          ) numbers
      ) numbers
  ) numbers;;
(*  - : int = 218767230 *)
