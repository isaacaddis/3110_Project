open State
open Printf
open Player
open Controller
open Parser

(** [quit ()] prints a message and then exits the game. *)
let quit () =
  print_endline "Exited the game.";
  exit 0

(** [format_pts p] formats the points from a players hand. *)
let format_pts (pts: int) : string =
  "(" ^ (string_of_int pts) ^ " pts)"

(** [print_st b st] prints the state, and changes depending on if [st] is the
    players turn or the dealers turn with bool [b]. *)
let print_st (p_turn:bool) st =
  let player = player st in
  let dealer = dealer st in
  let player_hand_str = hand_as_string player in
  let dealer_msg =
    if p_turn then
      "Dealer's hand:\n" ^ hand_as_facedown_string dealer
    else
      "Dealer's hand: " ^ format_pts (points dealer) ^ "\n" ^ 
        hand_as_string dealer
  in
  let player_msg = "Your hand: " ^ format_pts (points player) ^ "\n" ^
    player_hand_str in
  print_endline player_msg;
  print_endline dealer_msg

(** [game_msg s] prints the game message for state [s] and returns
    the user input afterwards. *)
let game_msg (s : State.t) () =
  begin match s |> player |> points with
    | 9 | 10 | 11 when s |> player |> hand |> List.length = 2
      -> print_endline "[stand]/[hit]/[double down]/[quit]?" 
    | _ -> print_endline "[stand]/[hit]/[quit]?"
  end;
  print_string "> ";
  read_line ()

(** [game_double s] plays out game with state [s] after doubling down. *)
let game_double st =
  print_endline "";
  print_st false st;
  let status = check_st_d st in
  match status with
  | (DWin, _) -> print_endline "You won double!\n"; DWin
  | (DLoss, _) -> print_endline "You lost double!\n"; DLoss
  | (Draw, _) -> print_endline "You tied!\n"; Draw
  | _ -> failwith "game_double should only be called after double down."

(** [game_loop b p s] loops the game with state [s] and boolean for player
    turn [p], with player bet [b]. *)
let rec game_loop bet p_turn st =
  print_endline "";
  print_st p_turn st;
  let status = check_st p_turn st in
  match status with
  | (Blackjack, _) -> print_endline "You won!\n"; Blackjack
  | (Win, _) -> print_endline "You won!\n"; Win
  | (Draw, _) -> print_endline "You tied!\n"; Draw
  | (Loss, _) -> print_endline "You lost!\n"; Loss
  | (Next, Next) ->
    begin
      if p_turn then
        match Parser.parse (game_msg st ())  with
        | Quit -> quit ()
        | Hit -> let st' = step st Hit in game_loop bet true st'
        | Stand -> let st' = step st Stand in game_loop bet false st'
        | Double -> 
          let p_money = st |> player |> money in
          let d_money = st |> dealer |> money in
          if bet * 2 <= p_money && bet * 2 <= d_money then
            let st' = step st Double in game_double st'
          else
            if bet * 2 > p_money then
              (print_endline ("You cannot double down if you do not have more" ^ 
              " than double your bet money. Please select a different option.");
              game_loop bet p_turn st)
            else
              (print_endline ("Cannot double down if dealer does not have " ^ 
              "more than double the bet money. Please select a different " ^ 
              "option.");
              game_loop bet p_turn st)
        | Advice -> print_endline (get_advice st); game_loop bet p_turn st
        | Unknown -> 
          print_endline ("Unknown command. Please select a command from the " ^
            "options above.");
          game_loop bet p_turn st
        else let st' = step st Stand in game_loop bet false st'
    end
  | _ -> failwith "an unexpected error occured"

(** [read_bet inp] runs the game state if [inp] is a proper integer bet,
    and asks for reinput if the [inp] is not appropriate.*)
let rec read_bet inp =
  if inp = "quit" then quit();
  try int_of_string inp
  with (Failure _) ->
    print_endline ("I could not understand that input. Please input an " ^
      "integer number of dollars to bet.");
    print_string "$";
    read_bet (read_line ())

(** [print_result p] prints the result depending on the dollars that you
    gained or lost. *)
let print_result pts =
  let p = int_of_float pts in
  if p = 0 then print_endline "You did not win or lose any money."
  else if p < 0 then 
    print_endline ("You lost " ^ string_of_int (-p) ^ " dollars.")
  else print_endline ("You won " ^ string_of_int p ^ " dollars.")

(** [check_bankrupt m] quites the game if [m] is equal to 0. *)
let check_bankrupt money =
  if money = 0 then
    (print_endline ("\nYou gamble away your life savings. Your wife leaves " ^ 
    "you. You are a broken man.\n"); exit 0)
  else ()

(** [play_game b s] runs the Blackjack game with state [s]. *)
let rec play_game st () = 
  let player_money = st |> player |> money in
  let dealer_money = st |> dealer |> money in
  check_bankrupt player_money;
  print_string ("Welcome to blackjack! Get closer to 21 than the dealer" ^ 
    " without busting! You have $" ^ (string_of_int player_money) ^ 
    ". Enter bet amount in dollars:\n$");
  let bet = read_bet (read_line ()) in
  if bet <= player_money && bet <= dealer_money && bet > 0 then
    let result = game_loop bet true st in
    play_game (next_round st result bet) ()
  else
    if bet > player_money then
      (print_endline "You cannot bet more than you have. Please try again.\n";
      play_game st ())
    else if bet > dealer_money then
      (print_endline ("You cannot bet more than the dealer has. Please bet " ^ 
      "lower, or switch to another dealer.\n"); play_game st ())
    else
      print_endline ("You must bet more than 0 dollars. Please try again.\n");
      play_game st ()

let start_state = init_state (get_stats ())

(** [main ()] runs the game, and gives the player instructions. *)
let main () = play_game start_state ()

let () = main ()
