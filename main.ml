open State
open Printf
open Player
open Controller

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

let print_check =
  print_endline "[stand]/[hit]/[quit]?";
  print_string "> ";
  read_line ()

let rec play_game st () = 
  print_endline "Now playing.";
  let condition = check_st st in
  match condition with
  | (Win, _) -> print_endline "You won."; play_game print_init_st ()
  | (Loss, _) -> print_endline "You lost."; play_game print_init_st ()
  | (Draw, _) -> 
      begin
        print_endline "Draw! No loss or gain."; 
        play_game print_init_st ()
      end
  | (Next x, Stop y) -> let r = parse print_check in play_game (step_st st r) ()
  | (Next x, Next y) -> let r = parse print_check in play_game (step_st st r) ()
  | _ -> print_endline "You lost."; play_game print_init_st ()

let main () = 
  print_endline "Welcome to blackjack! Get closer to 21 than the dealer without
    busting!";
  let st = init_state in
  play_game st ()

let () = main ()
