type commands = Stand | Hit | Quit | Double | Advice | Unknown

let parse str =
  match String.lowercase_ascii str with
  | "stand" -> Stand
  | "hit" -> Hit
  | "quit" -> Quit
  | "double down" -> Double
  | "advice" -> Advice
  | _ -> Unknown
