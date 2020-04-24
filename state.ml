open Deck
open Player

type t = {deck: deck, dealer: dealer, player:player}

let init_state = () (** TODO **)

let dealer s =
  s.dealer

let player s =
  s.player
