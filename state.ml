open Card
open Deck
open Player
open Parser
open Printf

type t = {deck: deck; dealer: Player.t; player: Player.t}

let init_state money= 
  let d = shuffle_deck () in
  let d' = draw_two_cards d in
  let d'' = d' |> deck |> draw_two_cards in
  { deck = (deck d''); dealer = (make_player (cards d') 50000); player = 
    (make_player (cards d'') money) }

(** [pm s] is the player's money in state [s]*)
let pm s = s.player |> money

(** [dm s] is the dealer's money in state [s]. *)
let dm s = s.dealer |> money

(** [pause s] pauses, prints a period, and decrements [s] until it is 0. *)
let rec pause t =
  if t <= 0 then print_endline ""
  else (print_endline "."; Unix.sleep 1; pause (t-1))

(** [dealer_draw s] is the state [s] becomes after the dealer draws a card. *)
let dealer_draw s = 
  let d = s.deck in
  let d_points = points s.dealer in
  pause 3;
  if d_points >= 17 then s else
    let r = draw_card d in
    {deck = (deck r); dealer = (make_player (List.append (hand s.dealer)
      (cards r)) (dm s)); player = s.player}

let step s cmd = 
  let d = s.deck in
  let r = draw_card d in
  match cmd with
  | Hit ->
    let new_s = { deck = (deck r); dealer = s.dealer; 
      player = make_player (List.append (hand s.player) (cards r)) 
      (money s.player) } in
    if points new_s.player = 21 then dealer_draw new_s
    else new_s
  | Stand -> dealer_draw s
  | Double -> 
    dealer_draw { deck = (deck r); dealer = s.dealer; 
      player = make_player (List.append (hand s.player) (cards r))
      (money s.player) }
  | Quit -> failwith "unimplemented"
  | _ -> failwith "unimplemented"

(** [x1_5 n] multiplies int [n] by 1.5, then reconverts to int. *)
let x1_5 num = (1.5 *. float_of_int num) |> int_of_float

(** [repl_m s m] is state [s] but with the player's money replaced with [m]*)
(*let repl_m s m =
  { deck = s.deck;
    dealer = s.dealer;
    player = make_player (s.player |> hand) m }*)

(** [update_money m] updates the stats.json file with new money value [m]. *)
let update_money money =
  let file = open_out "stats.json" in
  let out_string = "{ \n\t\"money\":" ^ string_of_int money ^ "\n}" in
  fprintf file "%s\n" out_string; close_out file; money

(** [calc_money s w b] calculates how much the player wins or loses based on
    bet [b] and condition [w]. *)
let calc_money s win bet =
  match win with
    | 1 -> x1_5 bet
    | 2  -> bet
    | 3 -> -bet
    | 4 -> 0
    | 5 -> bet * 2
    | 6 -> bet * -2
    | _ -> failwith "step_round received invalid input"

let step_round s win bet = 
  let d = shuffle_deck () in
  let d' = draw_two_cards d in
  let d'' = d' |> deck |> draw_two_cards  in
  let money = calc_money s win bet in
  { deck = (deck d''); 
    dealer = (make_player (cards d') (dm s - money)); 
    player = (make_player (cards d'') (update_money (pm s + money))) }


let dealer s =
  s.dealer

let player s = 
  s.player
