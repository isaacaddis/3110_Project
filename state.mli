(** Abstract value type of state. *)
type t

(** [init_state n] is the starting state of a round of black jack,
    where the dealer and the player have two cards each*)
val init_state: int -> t

(** [player t] returns the player from state [t]. *)
val player: t -> Player.t

(** [dealer t] returns the dealer from state [t]. *)
val dealer: t -> Player.t

(** [step s cmd] is the result of player choice [cmd] while in state [s]*)
val step : t -> Parser.commands -> t

(** [step_round s win bet] is a fresh round of blackjack, with the player's
    money updated according to the result [win] and wager [bet].
    These are what the possible values of [win] correspond to:
    1: Blackjack victory,
    2: Normal Victory,
    3: Loss,
    4: Draw,
    5: Double down victory,
    6: Double down loss,
    Other: Causes a failure*)
val step_round : t -> int -> int -> t 

(** [get_advice s] gives a string of advice to the player based on [s]. *)
val get_advice : t -> string