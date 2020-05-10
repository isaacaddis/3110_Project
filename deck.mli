(** Abstract type representing a deck. **)
type deck

(** Abstract type representing cards drawn from deck and remaining cards
    left in the deck. *)
type res

(** [cards res] returns the cards drawn in [res]. *)
val cards : res -> Card.t list

(** [shuffle_deck d] is a full 52-card shuffled deck.  *)
val shuffle_deck : unit -> deck

(** [draw_two_cards d] returns a result containing the two cards drawn, and
    the remaining cards in a separate deck. *)
val draw_two_cards : deck -> res

(** [deck r] returns the deck remaining from result [res]. *)
val deck : res -> deck

(** [draw_card d] returns a result of a card drawn from [d] and the cards
    left in the deck afterwards. *)
val draw_card : deck -> res

(** [cards_to_string r] returns a string list of the cards drawn in [r]. *)
val cards_to_string : res -> string list

(** [test_deck c] creates a non-randomized deck based off the int in pairs [c]
    representing individual cards*)
val test_deck : (int * int) list -> deck