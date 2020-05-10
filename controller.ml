open Player
open State

type pts = int
type condition = Bust | Natural | Int of pts
type win_condition = Blackjack | DWin | DLoss | Win | Draw | Loss | Next
type win = win_condition * win_condition

(** [get_player_condition p] returns the condition of the player's current
    hand. *)
let get_player_condition player =
  match points player with
  | 21 -> Natural
  | n when n > 21 -> Bust
  | n -> Int n

(** [check_st b s] checks if either the player or dealer has won yet, with
    boolean [b] representing if it is the player's turn or not. *)
let check_st p_turn st =
  let player = player st in
  let dealer = dealer st in
  let p_cond = get_player_condition player in
  let d_cond = get_player_condition dealer in
  match p_cond with
  | Bust -> (Loss, Win)
  | Natural when p_turn ->
    begin
      match d_cond with
      | Natural -> (Draw, Draw)
      | _ -> (Blackjack, Loss)
    end
  | Natural -> begin
      match d_cond with
      | Natural -> (Draw, Draw)
      | Int y when y >= 17 -> (Win, Loss)
      | Bust -> (Win, Loss)
      | Int y -> (Next, Next)
    end
  | Int x ->
    if not p_turn then
      begin match d_cond with
        | Natural -> (Loss, Win)
        | Bust -> (Win, Loss)
        | Int y ->
          if y >= 17 then
            begin
              if x > y then (Win, Loss)
              else if x = y then (Draw, Draw)
              else (Loss, Win)
            end
          else (Next, Next)
      end
    else if d_cond = Natural then
      (print_endline ""; print_endline "Dealer has a blackjack!";
       print_endline ("Your hand: " ^ hand_as_string player);
       print_endline ("Dealer's hand: " ^ hand_as_string dealer);
       (Loss, Win))
    else (Next, Next)

let check_st_d' (player: Player.t) (dealer: Player.t) =
  let p_cond = get_player_condition player in
  let d_cond = get_player_condition dealer in
  match p_cond with
  | Bust -> (DLoss, Win)
  | Natural -> begin
      match d_cond with
      | Natural -> (Draw, Draw)
      | Int y -> (DWin, Loss)
      | Bust -> (DWin, Loss)
    end
  | Int x ->
    match d_cond with
      | Natural -> (DLoss, Win)
      | Bust -> (DWin, Loss)
      | Int y ->
        if x > y then (DWin, Loss)
        else if x = y then (Draw, Draw)
        else (DLoss, Win)

(** [check_st_d s] checks if the player or dealer has won in a double down. *)
let check_st_d st =
  let player = player st in
  let dealer = dealer st in
  check_st_d' player dealer

let next_round s win bet = match win with
  | Blackjack -> step_round s 1 bet
  | Win -> step_round s 2 bet
  | Loss -> step_round s 3 bet
  | Draw -> step_round s 4 bet
  | DWin -> step_round s 5 bet
  | DLoss -> step_round s 6 bet
  | Next -> step_round s 7 bet
