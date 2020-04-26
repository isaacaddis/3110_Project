open State
open Printf
open Player

let format_pts (pts: int) : string =
  "(" ^ (string_of_int pts) ^ " pts)"

let print_init_st st =
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
  print_endline (player_msg ^ (format_pts (points player)));
  print_endline dealer_msg

let play_game = 
  let st = init_state in
  print_init_st st


let main () = 
  print_endline "Welcome to blackjack!";
  play_game

let () = main ()
