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
val top_card : t -> string

(** [hand_as_string p] represents the hand of [p] as a string. *)
val hand_as_string : t -> string

