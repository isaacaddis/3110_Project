(** Abstract type representing a card *)
type card

(** [get_val c] is the point value of [c] *)
val get_val : card -> int

(** [to_string c] is the card type of [c] as a string *)
val to_string : card -> string

(** [make_card f n] initializes a card with face value [f] and the [n]th
    time the card has been made, corresponding to one of for suits.
    Requires: [f] is in [1..13], 
              [n] is in [1...4] *)
val make_card : int -> int -> card
