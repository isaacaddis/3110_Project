open Card
open Deck
open Player


type t = {deck: deck; dealer: player; player: player}

let init_state =
  let d = shuffle_deck in
  let d' = draw_two_cards d in
  let d'' = draw_two_cards (deck d') in
  { deck = (deck d''); dealer = { hand = cards d' }; 
    player = { hand = cards d'' } }

let step s = 
  let d = s.deck in
  let r = draw_card d in
  { deck = (deck r); dealer = { hand = hand s.dealer }; 
    player = { hand = cards r } }

let dealer s =
  s.dealer

let player s = 
  s.player
