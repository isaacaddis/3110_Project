open State
open Printf
open Player
open Controller
open Parser

let quit () =
  print_endline "Exited the game.";
  exit 0

let format_pts (pts: int) : string =
  "(" ^ (string_of_int pts) ^ " pts)"

let print_st (initial_run:bool) st =
  let player = player st in
  let dealer = dealer st in
  let player_hand = hand player in
  let player_hand_str = {hand = player_hand }|> hand_as_string 
    |>  String.concat ", " in
  let dealer_hand = hand dealer in
  let dealer_hand_str =
    begin
      if initial_run = true then
        ({hand = dealer_hand }|> hand_as_string |> List.hd) ^ ", and another 
          face down."
      else
        {hand = dealer_hand }|> hand_as_string |> String.concat ", "
    end
  in
  let player_msg = "Your hand: " ^ player_hand_str in
  let dealer_msg = "Dealer's hand: " ^ dealer_hand_str in
  print_endline (player_msg ^ " " ^ (format_pts (points player)));
  print_endline dealer_msg

let game_msg () =
  print_endline "[stand]/[hit]/[quit]?";
  print_string "> ";
  read_line ()

let rec game_loop initial_run st =
  if initial_run = true then () else print_st false st;
  let status = check_st initial_run st in
  match status with
  | (Win, _) -> print_endline "You won!"; ()
  | (Draw, _) -> print_endline "You tied!"; ()
  | (Loss, _) -> print_endline "You lost!"; ()
  | (Next, Next) ->
      begin
        match Parser.parse (game_msg ())  with
        | Quit -> quit ()
        | Hit -> let st' = step st Hit in game_loop false st'
        | Stand -> let st' = step st Stand in game_loop false st'
        | Unknown -> print_endline "unknown command"; quit ()
      end
  | _ -> failwith "an unexpected error occured"

let play_game () = 
  let st = init_state in
  print_st true st;
  game_loop true st

let main () = 
  print_endline "Welcome to blackjack! Get closer to 21 than the dealer without
    busting!";
  play_game ()

let () = main ()
