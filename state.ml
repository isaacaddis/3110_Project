open Card
open Deck
open Player
open Parser


type t = {deck: deck; dealer: player; player: player}
type users = Player | Dealer

let init_state = 
  let d = shuffle_deck in
  let d' = draw_two_cards d in
  let d'' = d' |> deck |> draw_two_cards  in
  { deck = (deck d''); dealer = { hand = cards d' }; 
    player = { hand = cards d'' } }

(** [dealer_draw s] is the state [s] becomes after the dealer draws their cards*)
let rec dealer_draw s = 
  let d = s.deck in
  let d_points = points s.dealer in
  if d_points >= 17 then s else
    let r = draw_card d in
    dealer_draw {deck = (deck r); 
                 dealer = {hand = (cards r) @ (hand s.dealer)}; 
                 player = s.player}

let step s cmd = 
  let d = s.deck in
  let r = draw_card d in
  match cmd with
  | Hit -> let s' = { deck = (deck r); 
                      dealer = s.dealer; 
                      player = { hand = (cards r) @ (hand s.player) }} in
    begin match points s'.player with
      | 21 -> dealer_draw s'
      | _ -> s' 
    end
  | Stand -> dealer_draw s
  | Double -> dealer_draw { deck = (deck r); 
                            dealer = s.dealer; 
                            player = { hand = (cards r) @ (hand s.player) }}
  | Quit -> failwith "unimplemented"
  | _ -> failwith "unimplemented"

let dealer s =
  s.dealer

let player s = 
  s.player
