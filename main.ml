open State

let print_init_st =
  let player = player st in
  let dealer = dealer st in
  let player_hand = hand player in
  let dealer_hand = hand dealer |> List.hd in
  print_endline "Your hand: "

let play_game = 
  let st = init_state in
  print_init_st st


let main () = 
  ANSITerminal.(print_string [green] "\n Welcome to blackjack. \n");
  play_game


