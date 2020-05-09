open State
open Printf
open Player
open Controller
open Parser
open Yojson.Basic.Util

(** [get_stats ()] searches for the file 'stats.json' to get the money for
    the player, and if it doesn't exist, it makes a file 'stats.json'. *)
let get_stats () =
  if Sys.file_exists "stats.json" then
    (** read in data from stats *)
    let file = Yojson.Basic.from_file "stats.json" in
    file |> to_assoc |> List.assoc "money" |> to_int
  else
    let file = open_out "stats.json" in
    fprintf file "%s\n" "{\n\t\"money\": 500\n}"; close_out file; 1000

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
  let dealer_hand_str =
    begin
      if p_turn then
        (top_card dealer) ^ ", and another face down."
      else
        hand_as_string dealer
    end
  in
  let player_msg = "Your hand: " ^ player_hand_str in
  let dealer_msg = "Dealer's hand: " ^ dealer_hand_str in
  print_endline (player_msg ^ " " ^ (format_pts (points player)));
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

(** Returns: point multipler (0., 1.5, -1, 1) *)
let rec game_loop p_turn st =
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
      match Parser.parse (game_msg st ())  with
      | Quit -> quit ()
      | Hit -> let st' = step st Hit in game_loop true st'
      | Stand -> let st' = step st Stand in game_loop false st'
      | Double -> let st' = step st Double in game_loop false st'
      | Unknown -> 
        print_endline ("Unknown command. Please select a command from the " ^
                       "options above.");
        game_loop p_turn st
    end
  | _ -> failwith "an unexpected error occured"

(** [read_bet inp] runs the game state if [inp] is a proper integer bet,
    and asks for reinput if the [inp] is not appropriate.*)
let rec read_bet inp =
  try int_of_string inp
  with (Failure e) ->
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

(** [play_game b] runs the Blackjack game with a bet [b]. *)
let rec play_game st () = 
  let player_money = st |> player |> money in
  print_string ("Welcome to blackjack! Get closer to 21 than the dealer without
    busting! You have $" ^ (string_of_int player_money) ^ 
    ". Enter bet amount in dollars:\n$");
  let bet = read_bet (read_line ()) in
  if bet <= player_money then
    let result = game_loop true st in
    play_game (next_round st result bet) ()
  else 
    print_endline "You cannot bet more than you have. Please try again.\n";
    play_game st ()

let start_state = init_state (get_stats ())

(** [main ()] runs the game, and gives the player instructions. *)
let main () = play_game start_state ()

let () = main ()
