let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s;;

let numbers = read_file "01_input.txt"
              |> String.split_on_char '\n'
              |> List.filter (fun s -> not (String.equal s ""))
              |> List.map int_of_string;;


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
