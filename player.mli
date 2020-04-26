(** Abstract value type of player *)
type player = { hand: Card.card list }

(** [points p] gets the point value of [p] *)
val points : player -> int

(** [hand p] gets the hand of [p] *)
val hand : player -> Card.card list

(** [hand_as_string h] represents [h] as a list of strings *)
val hand_as_string : player -> string list

