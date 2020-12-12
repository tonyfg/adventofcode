#use "helpers.ml";;

let places =
  read_lines "11_input.txt"
  |> List.map (String.to_seq >> Array.of_seq)
  |> Array.of_list;;
let width = Array.length places.(0);;
let height = Array.length places;;


let value matrix x y = matrix.(y).(x)

let state matrix x y =
  if x < 0 || x >= width || y < 0 || y >= height then
    None
  else
    Some (value matrix x y);;

let busy matrix x y = state matrix x y = Some '#';;


(* Part 1 *)
let busy_around matrix x y =
  let sum_busy sum (x_off, y_off) =
    if busy matrix (x + x_off) (y + y_off) then sum + 1
    else sum in
  [(-1, -1); (-1, 0); (-1, 1);
   (0, -1);           (0, 1);
   (1, -1);  (1, 0);  (1, 1)]
  |> List.fold_left sum_busy 0;;

let rec calc_seats ?(rows = places) ~max_busy ~busy_around () =
  let calc_seat x y =
    match (state rows x y, busy_around rows x y) with
    | (Some 'L', 0) -> '#'
    | (Some '#', n) when n > max_busy -> 'L'
    | _ -> value rows x y in
  let calc_row y = Array.mapi (fun x _ -> calc_seat x y) in
  let new_rows = Array.mapi calc_row rows in
  if rows = new_rows then rows
  else calc_seats ~max_busy ~rows:new_rows ~busy_around ();;

let add_busy_seats ?(busy_around = busy_around) sum row =
  let add_busy_seat sum seat =
    if seat = '#' then sum + 1 else sum in
  Array.fold_left add_busy_seat sum row;;

calc_seats ~max_busy:3 ~busy_around ()
|> Array.fold_left add_busy_seats 0;;
(* - : int = 2299 *)


(* Part 2 *)
let busy_far matrix x y =
  let rec find_seat ?(mul = 1) (x2, y2) =
    let new_x = (x + (x2 * mul))
    and new_y = (y + (y2 * mul)) in
    match state matrix new_x new_y with
    | Some '.' -> find_seat ~mul:(mul + 1) (x2, y2)
    | Some x -> (new_x, new_y)
    | _ -> (new_x, new_y) in
  let sum_busy sum (x_off, y_off) =
    let (seat_x, seat_y) = find_seat (x_off, y_off) in
    if busy matrix seat_x seat_y then sum + 1
    else sum in
  [(-1, -1); (-1, 0); (-1, 1);
   (0, -1);           (0, 1);
   (1, -1);  (1, 0);  (1, 1)]
  |> List.fold_left sum_busy 0;;

calc_seats ~max_busy:4 ~busy_around:busy_far ()
|> Array.fold_left add_busy_seats 0;;
(* - : int = 2047 *)
