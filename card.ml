type suit = Diamonds | Spades | Hearts | Clubs
type card' = Jack | Queen | King | Ace | Int of face_value
type card = (int * card' * suit)

let suit_to_string s =
  match s with
  | Diamonds -> "Diamonds"
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Clubs -> "Clubs"

let card_to_string (card': card') : string =
  match card' with
  | Jack  -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"
  | Int xs -> string_of_int xs

let to_string c =
  card_to_string (snd c)

let match_suit s =
  match s with
  | 1 -> Diamonds
  | 2 -> Spades
  | 3 -> Hearts
  | 4 -> Clubs

let make_card v n =
  let s = match_suit n in
  match v with
  | 12 -> (10, King, s)
  | 11 -> (10, Queen, s)
  | 10 -> (10, Jack, s)
  | 1 -> (1, Ace, s)
  | x -> (x, Int x, s)
