(** Abstract value type of state. *)
type t

(** [init_state n] is the starting state of a round of black jack,
    where the dealer and the player have two cards each*)
val init_state: t

(** [player t] returns the player from state [t]. *)
val player: t -> Player.t

(** [dealer t] returns the dealer from state [t]. *)
val dealer: t -> Player.t

(** [step s cmd] is the result of player choice [cmd] while in state [s]*)
val step : t -> Parser.commands -> t

(** [step_round s win bet] is a fresh round of blackjack, with the player's
    money updated according to the result [win] and wager [bet]*)
val step_round : t -> Controller.win_condition -> int -> t 