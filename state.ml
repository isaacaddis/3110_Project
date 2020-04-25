open Card
open Deck
open Player


type t = {deck: deck; dealer: player; player: player}

let thrd = (fun (_,_,z) -> z)
let combine = (fun (x,y,_) -> x :: y :: [])

let init_state =
  let d = shuffle_deck in
  let d' = draw_two_cards (d) in
  let d'' = draw_two_cards (deck d') in
  { deck = (deck d''); dealer = { hand = cards_to_string d' }; 
    player = { hand = cards_to_string d'' } }

let dealer s =
  s.dealer

let player s =
  s.player
