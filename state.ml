open Card
open Deck
open Player
open Parser


type t = {deck: deck; dealer: player; player: player}
type users = Player | Dealer

let init_state =
  let d = shuffle_deck in
  let d' = draw_two_cards d in
  let d'' = draw_two_cards (deck d') in
  { deck = (deck d''); dealer = { hand = cards d' }; 
    player = { hand = cards d'' } }

let step s cmd = 
  let dealer = s.dealer in
  let d_points = points dealer in
  let d = s.deck in
  let r = draw_card d in
  let r' = draw_card (deck r) in
  match d_points with
  | n when n < 17 ->
    if cmd = Hit then
    { deck = (deck r'); dealer = { hand = (cards r) @ (hand s.dealer) }; 
      player = { hand = (cards r') @ (hand s.player) } }
    else
    { deck = (deck r); dealer = { hand = (cards r) @ (hand s.dealer) }; 
      player = { hand = hand s.player} }
  | _ ->
    if cmd = Hit then
    { deck = (deck r); dealer = { hand = hand s.dealer}; 
      player = { hand = (cards r) @ (hand s.player)} }
    else
      s


let dealer s =
  s.dealer

let player s = 
  s.player
