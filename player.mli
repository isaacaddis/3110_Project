(** 
   Representation of player data.

   This module represents the data stored in the player, including the money
   he/she has, the point value of his/her hand, and the cards that he/she has.
*)

(** Abstract value type of player. *)
type t

(** [make_player h m] instantiates player with hand [h] and money [m]. *)
val make_player : Card.t list -> int -> t

(** [points p] gets the point value of the hand of [p]. *)
val points : t -> int

(** [money p] gets the money of [p]. *)
val money : t -> int

(** [hand p] gets the hand of [p]. *)
val hand : t -> Card.t list

(** [top_card p] represents the top card that [p] has as a string. *)
val top_card : t -> Card.t

(** [top_card p] represents a two-card hand with one hidden as ASCII art. *)
val hand_as_facedown_string : t -> string

(** [hand_as_string p] represents the hand of [p] as ASCII art. *)
val hand_as_string : t -> string

(** [only_has_cards p v1 v2] checks if player [p] only has [v1] and [v2]. *)
val only_has_cards : t -> int -> int -> bool