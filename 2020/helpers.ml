let read_lines filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s |> String.split_on_char '\n'
    |> List.filter (fun s -> not (s = ""));;

let (>>) f g x = g(f x)

let matches regex s = Str.string_match (Str.regexp regex) s 0;;

module S = struct
  let range a b = Seq.unfold (fun i -> if i < b then Some(i, i + 1) else None) a
end

module L = struct
  let min = List.fold_left min Int.max_int;;
  let max = List.fold_left max 0;;
  let sum = List.fold_left (+) 0;;
end

module A = struct
  let min = Array.fold_left min Int.max_int;;
  let max = Array.fold_left max 0;;
  let sum = Array.fold_left (+) 0;;
end
