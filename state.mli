type t

type users = Player | Dealer

val init_state: t

val player: t -> Player.player

val dealer: t -> Player.player

val step : t -> Parser.commands -> t
