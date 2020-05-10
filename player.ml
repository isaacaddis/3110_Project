open Card

type t = { hand: Card.t list; money: int }

let make_player hand money =
  { hand = hand; money = money}

(** [p_help hnd acc] is a helper function that returns a pair containing the 
    number of aces in a card list and the point value of the non ace cards*)
let rec p_help hnd acc =
  match hnd with
  | [] -> acc
  | h :: t when get_val h = 1 -> p_help t ((fst acc) + 1, snd acc)
  | h :: t -> p_help t (fst acc, snd acc + get_val h)

(** [points p] is the highest non-bust value of points that [p] has. *)
let points p = 
  let hnd = p.hand in
  let v = p_help hnd (0, 0) in
  match fst v with
  | 0 -> snd v
  | x when 11 + (x - 1 + snd v) > 21 -> x + snd v
  | x -> 11 + (x - 1 + snd v)

let money p =
  p.money

let hand p =
  p.hand

let top_card p =
  let hand = hand p in
  match hand with
  | h::t -> h
  | _ -> failwith "Tried to get top card of player with empty hand"

let hand_as_facedown_string p =
  card_facedown_to_ascii_string (hand p)

let hand_as_string p =
  let hand = hand p in
  cards_to_ascii_string hand

let only_has_cards p v1 v2 =
  let p_vals = List.map get_val p.hand in
  (List.mem v1 p_vals) && (List.mem v2 p_vals) && List.length p_vals = 2