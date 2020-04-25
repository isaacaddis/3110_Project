open State
open Printf
open Player

let print_init_st st =
  let player = player st in
  let dealer = dealer st in
  let player_hand = hand player |> String.concat ", " in
  let dealer_hand = hand dealer |> List.hd in
  let player_msg = "Your hand: " ^ player_hand in
  let dealer_msg = "Dealer's hand: " ^ dealer_hand ^ ", and another facedown" in
  print_endline player_msg;
  print_endline dealer_msg

let play_game = 
  let st = init_state in
  print_init_st st


let main () = 
  print_endline "Welcome to blackjack!";
  play_game

let () = main ()
