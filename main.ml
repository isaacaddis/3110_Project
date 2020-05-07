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

(** [print_st i st] prints the state, and changes depending on if [st] is the
    players turn or the dealers turn with bool [b]. *)
let print_st (initial_run:bool) st =
  let player = player st in
  let dealer = dealer st in
  let player_hand_str = player |> hand_as_string |> String.concat ", " in
  let dealer_hand_str =
    begin
      if initial_run = true then
        (dealer |> hand_as_string |> List.hd) ^ ", and another 
          face down."
      else
        dealer |> hand_as_string |> String.concat ", "
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
let rec game_loop initial_run st =
  if initial_run = true then () else print_st false st;
  let status = check_st initial_run st in
  match status with
  | (Blackjack, _) -> print_endline "You won!"; Blackjack
  | (Win, _) -> print_endline "You won!"; Win
  | (Draw, _) -> print_endline "You tied!"; Draw
  | (Loss, _) -> print_endline "You lost!"; Loss
  | (Next, Next) ->
    begin
      match Parser.parse (game_msg st ())  with
      | Quit -> quit ()
      | Hit -> let st' = step st Hit in game_loop false st'
      | Stand -> let st' = step st Stand in game_loop false st'
      | Double -> let st' = step st Double in game_loop false st'
      | Unknown -> 
        print_endline ("Unknown command. Please select a command from the " ^
         "options above.");
        game_loop initial_run st
    end
  | _ -> failwith "an unexpected error occured"

(** [play_game b] runs the Blackjack game with a bet [b]. *)
let play_game (bet: int) () = 
  let st = init_state in
  print_st true st;
  let bet = float_of_int bet in
  match game_loop true st with
  | Blackjack -> 1.5 *. bet
  | Win -> bet
  | Draw -> 0.
  | Loss -> -1. *. bet
  | _ -> failwith "an error occured"

(** [print_result p] prints the result depending on the dollars that you
    gained or lost. *)
let print_result pts =
  let p = int_of_float pts in
  if p = 0 then print_endline "You tied."
  else if p < 0 then 
    print_endline ("You lost " ^ string_of_int (-p) ^ " dollars.")
  else print_endline ("You won " ^ string_of_int p ^ " dollars.")

(** [read_bet inp] runs the game state if [inp] is a proper integer bet,
    and asks for reinput if the [inp] is not appropriate.*)
let rec read_bet inp =
  try int_of_string inp
  with (Failure e) ->
    print_endline ("I could not understand that input. Please input an " ^
      "integer number of dollars to bet.");
    print_string "$";
    read_bet (read_line ())

(** [main ()] runs the game, and gives the player instructions. *)
let main () = 
  print_endline "Welcome to blackjack! Get closer to 21 than the dealer without
    busting! Enter bet amount in dollars: ";
  print_string "$";
  let pts = play_game (read_bet (read_line ())) () in
  print_result pts

let () = main ()
