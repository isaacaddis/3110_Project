type commands = Stand | Hit | Quit | Double | Unknown

let parse str =
  match String.lowercase str with
  | "stand" -> Stand
  | "hit" -> Hit
  | "quit" -> Quit
  | "double down" -> Double
  | _ -> Unknown
