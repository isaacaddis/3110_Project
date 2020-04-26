open Card

type player = { hand: card list }

let rec pts = function
  | [] -> 0
  | x :: xs -> (get_val x) + (pts xs)

(** [p_help hnd acc] is a helper function that returns a pair containing the 
    number of aces in a card list and the point value of the non ace cards*)
let rec p_help hnd acc=  match hnd with
  | [] -> acc
  | h :: t when get_val h = 1 -> p_help t ((fst acc) + 1, snd acc)
  | h :: t -> p_help t (fst acc, snd acc + get_val h)

(** [points hnd] is the highest non-bust value of points in [hnd]*)
let points hnd = 
  let hnd = hnd.hand in
  let v = p_help hnd (0, 0) in
  match fst v with
  | 0 -> snd v
  | x when 11 + (x - 1 + snd v) > 21 -> x + snd v
  | x -> 11 + (x - 1 + snd v)

let hand p =
  p.hand 

let hand_as_string p =
  let hand = hand p in
  let rec as_string_list = function
    | [] -> []
    | x :: xs -> (to_string x) :: (as_string_list xs)
  in as_string_list hand

