type player = { hand: card list }

let init_player h =
  { hand = h }

let rec pts = function
  | [] -> 0
  | x :: xs -> x + (pts xs)

let hand p =
  p.hand
  
