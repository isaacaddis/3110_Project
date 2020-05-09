open Card
open Deck
open Player
open Parser

type t = {deck: deck; dealer: Player.t; player: Player.t}

let init_state = 
  let d = shuffle_deck () in
  let d' = draw_two_cards d in
  let d'' = d' |> deck |> draw_two_cards  in
  { deck = (deck d''); dealer = (make_player (cards d') 50000); player = 
                                                                  (make_player (cards d'') 500) }

(** [dealer_draw s] is the state [s] becomes after the dealer draws cards. *)
let rec dealer_draw s = 
  let d = s.deck in
  let d_points = points s.dealer in
  if d_points >= 17 then s else
    let r = draw_card d in
    dealer_draw {deck = (deck r); 
                 dealer = (make_player (cards r @ hand s.dealer) 50000); player = s.player}

let step s cmd = 
  let d = s.deck in
  let r = draw_card d in
  match cmd with
  | Hit ->
    let new_s = { deck = (deck r); dealer = s.dealer; 
                  player = make_player (cards r @ hand s.player) (money s.player) } in
    if points new_s.player = 21 then dealer_draw new_s
    else new_s
  | Stand -> dealer_draw s
  | Double -> 
    dealer_draw { deck = (deck r); dealer = s.dealer; 
                  player = make_player (cards r @ hand s.player) (money s.player) }
  | Quit -> failwith "unimplemented"
  | _ -> failwith "unimplemented"

let x1_5 num = (1.5 *. float_of_int num) |> int_of_float

(** [repl_m s m] is state [s] but with the player's money replaced with [m]*)
(*let repl_m s m =
  { deck = s.deck;
    dealer = s.dealer;
    player = make_player (s.player |> hand) m }*)

(** [pm s] is the player's money in state [s]*)
let pm s = s.player |> money

let step_round s win bet = 
  let d = shuffle_deck () in
  let d' = draw_two_cards d in
  let d'' = d' |> deck |> draw_two_cards  in
  { deck = (deck d''); 
    dealer = (make_player (cards d') 50000); 
    player = (make_player (cards d'') 
                begin
                  match win with
                  | 1 -> pm s + (x1_5 bet)
                  | 2  -> pm s + bet
                  | 3 -> pm s - bet
                  | 4 -> pm s
                  | 5 -> failwith "player's money cannot change during a round"
                  | _ -> failwith "step_round received invalid input"
                end) }


let dealer s =
  s.dealer

let player s = 
  s.player
