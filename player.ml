open Card

type player = { hand: string list }

let rec pts = function
  | [] -> 0
  | x :: xs -> (get_val x) + (pts xs)

let hand (p: player) =
  p.hand 
