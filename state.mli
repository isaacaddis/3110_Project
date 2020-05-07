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
