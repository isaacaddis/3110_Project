open Deck
open Player

type t = {deck: deck, dealer: dealer, player:player}

let init_state =
  let d = shuffle_deck in
  let d' = draw_two_cards d in
  let d'' = draw_two_cards (snd d') in
  { deck = snd d''; dealer = fst d'; 
    player = fst d''}

let dealer s =
  s.dealer

let player s =
  s.player
