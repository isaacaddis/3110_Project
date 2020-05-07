(** Abstract type representing a deck. **)
type deck

(** Abstract type representing cards drawn from deck. *)
type res

val cards : res -> Card.t list

val shuffle_deck : deck

val draw_two_cards : deck -> res

val deck : res -> deck

val draw_card : deck -> res

val cards_to_string : res -> string list
