(** Abstract type representing a deck **)
type deck
type res

val shuffle_deck : deck

val draw_two_cards : deck -> res

val deck : res -> deck

val cards_to_string : res -> string list