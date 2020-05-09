(** Abstract type representing a card *)
type t

(** [get_val c] is the point value of [c]. *)
val get_val : t -> int

(** [cards_to_ascii_string h] turns a card list [h] into an ASCII string.*)
val cards_to_ascii_string : t list -> string

(** [card_Facedown_to_ascii_string h] turns a card list [h] into the top
    card face up and the second card face down, for an input of two cards. *)
val card_facedown_to_ascii_string : t list -> string

(** [to_string c] is the card type of [c] as a string. *)
val to_string : t -> string

(** [make_card f n] initializes a card with face value [f] and the [n]th
    time the card has been made, corresponding to one of for suits.
    Requires: [f] is in [1..13], 
              [n] is in [1...4] *)
val make_card : int -> int -> t
