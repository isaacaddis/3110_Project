type commands = Stand | Hit | Quit | Unknown

let parse str =
  match String.lowercase str with
  | "stand" -> Stand
  | "hit" -> Hit
  | "quit" -> Quit
  | _ -> Unknown
