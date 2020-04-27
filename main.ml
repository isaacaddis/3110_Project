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

let print_init_st =
  let st = init_state in
  let player = player st in
  let dealer = dealer st in
  let player_hand = hand player in
  let player_hand_str = {hand = player_hand }|> hand_as_string 
    |>  String.concat ", " in
  let dealer_hand = hand dealer in
  let dealer_hand_str = {hand = dealer_hand }|> hand_as_string |> List.hd in
  let player_msg = "Your hand: " ^ player_hand_str in
  let dealer_msg = "Dealer's hand: " ^ dealer_hand_str
    ^ ", and another facedown" in
  print_endline (player_msg ^ " " ^ (format_pts (points player)));
  print_endline dealer_msg;
  st

let game_msg =
  print_endline "[stand]/[hit]/[quit]?";
  print_string "> ";
  read_line ()

let rec game_loop st =
  let status = check_st st in
  match status with
  | (Win, _) -> print_endline "You won!"; ()
  | (Draw, _) -> print_endline "You tied!"; ()
  | (Loss, _) -> print_endline "You lost!"; ()
  | (Next, Next) ->
      begin
        match Parser.parse (game_msg)  with
        | Quit -> quit ()
        | Hit -> let st' = step st Hit in game_loop st'
        | Stand -> let st' = step st Stand in game_loop st'
        | Unknown -> print_endline "unknown command"; quit ()
      end
  | _ -> failwith "an unexpected error occured"

let play_game = 
  let st = init_state in
  print_endline "New round.";
  game_loop st

let main () = 
  print_endline "Welcome to blackjack! Get closer to 21 than the dealer without
    busting!";
  play_game

let () = main ()
