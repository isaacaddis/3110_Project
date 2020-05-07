type t

type users = Player | Dealer

(** [init_state n] is the starting state of a round of black jack,
    where the dealer and the player have two cards each*)
val init_state: t

val player: t -> Player.player

val dealer: t -> Player.player

(** [step s cmd] is the result of player choice [cmd] while in state [s]*)
val step : t -> Parser.commands -> t
