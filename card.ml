type face_value = int
type suit = Diamonds | Spades | Hearts | Clubs
type card = Jack | Queen | King | Ace | Int of face_value
type t = (int * card * suit)

let get_val (c: t) : int = 
  match c with
  | (i, _, _) -> i

let suit_to_string s =
  match s with
  | Diamonds -> "Diamonds"
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Clubs -> "Clubs"

let card_to_string (card: card) : string =
  match card with
  | Jack  -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"
  | Int xs -> string_of_int xs

let to_string c =
  match c with
  | (_, x, s) -> card_to_string x ^ " of " ^ suit_to_string s

let match_suit s =
  match s with
  | 1 -> Diamonds
  | 2 -> Spades
  | 3 -> Hearts
  | 4 -> Clubs
  | _ -> failwith "n must be in between 1 and 4, inclusive"

let make_card v n =
  let s = match_suit n in
  match v with
  | 12 -> (10, King, s)
  | 11 -> (10, Queen, s)
  | 10 -> (10, Jack, s)
  | 1 -> (1, Ace, s)
  | 0 -> (10,Int 10, s)
  | x -> (x, Int x, s)

