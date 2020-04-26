open Card

type player = { hand: string list }

let rec pts = function
  | [] -> 0
  | x :: xs -> (get_val x) + (pts xs)


(** [p_help hnd acc] is a helper function that returns a pair containing the 
    number of aces in a card list and the point value of the non ace cards*)
let rec p_help hnd acc=  match hnd with
  | [] -> acc
  | h :: t when get_val h = 1 -> p_help t ((fst acc) + 1, snd acc)
  | h :: t -> p_help t (fst acc, snd acc + get_val h)

(** [best_with_aces a b] is a helper function that returns the highest non-bust 
    point value of a hand with [a] aces and [b] non ace points*)
let rec best_with_aces a b = match a with
  | 0 -> b
  | x when x * 11 + b > 21 -> best_with_aces (x - 1) (b + 1) 
  | x -> x * 11 + b 

(** [points hnd] is the highest non-bust value of points in [hnd]*)
let points hnd = 
  let v = p_help hnd (0, 0) in
  best_with_aces (fst v) (snd v)

let hand (p: player) =
  p.hand 
