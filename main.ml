open State
open Printf
open Player

let print_init_st =
  let player = player st in
  let dealer = dealer st in
  let player_hand = hand player |> String.concat ", " in
  let dealer_hand = hand dealer |> String.concat ", " |> List.hd in
  let player_msg = "Your hand: " ^ player_hand in
  print_endline player_msg

let play_game = 
  let st = init_state in
  print_init_st st


let main () = 
  ANSITerminal.(print_string [green] "\n Welcome to blackjack. \n");
  play_game


