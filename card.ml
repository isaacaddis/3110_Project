type suit = Diamonds | Spades | Hearts | Clubs

let suit_to_string s =
  match s with
  | Diamonds -> "Diamonds"
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Clubs -> "Clubs"

type card = (int * suit)

let to_string c =
  match c with
  | (v, s) when v <= 10 && v >= 2 ->
    Int.to_string v ^ " of " ^ suit_to_string s
  | (v, s) when v = 1 ->
    "Ace of " ^ suit_to_string s
  | (v, s) when v = 11 ->
    "Jack of " ^ suit_to_string s
  | (v, s) when v = 12 ->
    "Queen of " ^ suit_to_string s
  | (v, s) when v = 13 ->
    "King of " ^ suit_to_string s
  | _ -> failwith "Integer not between 1 and 13"

(** value is between 1 and 52 *)
let make_card value =
  match value with
  | v when v mod 4 = 0 -> (v / 4, Diamonds)
  | v when v mod 4 = 1 -> (v / 4, Spades)
  | v when v mod 4 = 2 -> (v / 4, Hearts)
  | v when v mod 4 = 3 -> (v / 4, Clubs)
  | _ -> failwith "failure"