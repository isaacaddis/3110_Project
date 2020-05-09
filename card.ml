type face_value = int
type suit = Diamonds | Spades | Hearts | Clubs
type card = Jack | Queen | King | Ace | Int of face_value
type t = (int * card * suit)

(** ASCII art taken from https://www.asciiart.eu/miscellaneous/playing-cards.*)
let spades = [
  [" _____ "; "|A .  |"; "| /.\\ |"; "|(_._)|"; "|  |  |"; "|____V|"];
  [" _____ "; "|2    |"; "|  ^  |"; "|     |"; "|  ^  |"; "|____Z|"];
  [" _____ "; "|3    |"; "| ^ ^ |"; "|     |"; "|  ^  |"; "|____E|"];
  [" _____ "; "|4    |"; "| ^ ^ |"; "|     |"; "| ^ ^ |"; "|____h|"];
  [" _____ "; "|5    |"; "| ^ ^ |"; "|  ^  |"; "| ^ ^ |"; "|____S|"];
  [" _____ "; "|6    |"; "| ^ ^ |"; "| ^ ^ |"; "| ^ ^ |"; "|____9|"];
  [" _____ "; "|7    |"; "| ^ ^ |"; "|^ ^ ^|"; "| ^ ^ |"; "|____L|"];
  [" _____ "; "|8    |"; "|^ ^ ^|"; "| ^ ^ |"; "|^ ^ ^|"; "|____8|"];
  [" _____ "; "|9    |"; "|^ ^ ^|"; "|^ ^ ^|"; "|^ ^ ^|"; "|____6|"];
  [" _____ "; "|10 ^ |"; "|^ ^ ^|"; "|^ ^ ^|"; "|^ ^ ^|"; "|___0I|"];
  [" _____ "; "|J  ww|"; "| ^ {)|"; "|(.)% |"; "| | % |"; "|__%%[|"];
  [" _____ "; "|Q  ww|"; "| ^ {(|"; "|(.)%%|"; "| |%%%|"; "|_%%%O|"];
  [" _____ "; "|K  WW|"; "| ^ {)|"; "|(.)%%|"; "| |%%%|"; "|_%%%>|"]
]

let clubs = [
  [" _____ "; "|A _  |"; "| ( ) |"; "|(_'_)|"; "|  |  |"; "|____V|"];
  [" _____ "; "|2    |"; "|  &  |"; "|     |"; "|  &  |"; "|____Z|"];
  [" _____ "; "|3    |"; "| & & |"; "|     |"; "|  &  |"; "|____E|"];
  [" _____ "; "|4    |"; "| & & |"; "|     |"; "| & & |"; "|____h|"];
  [" _____ "; "|5    |"; "| & & |"; "|  &  |"; "| & & |"; "|____S|"];
  [" _____ "; "|6    |"; "| & & |"; "| & & |"; "| & & |"; "|____9|"];
  [" _____ "; "|7    |"; "| & & |"; "|& & &|"; "| & & |"; "|____L|"];
  [" _____ "; "|8    |"; "|& & &|"; "| & & |"; "|& & &|"; "|____8|"];
  [" _____ "; "|9    |"; "|& & &|"; "|& & &|"; "|& & &|"; "|____6|"];
  [" _____ "; "|10 & |"; "|& & &|"; "|& & &|"; "|& & &|"; "|___0I|"];
  [" _____ "; "|J  ww|"; "| o {)|"; "|o o% |"; "| | % |"; "|__%%[|"];
  [" _____ "; "|Q  ww|"; "| o {(|"; "|o o%%|"; "| |%%%|"; "|_%%%O|"];
  [" _____ "; "|K  WW|"; "| o {)|"; "|o o%%|"; "| |%%%|"; "|_%%%>|"]
]

let hearts = [
  [" _____ "; "|A_ _ |"; "|( v )|"; "| \\ / |"; "|  .  |"; "|____V|"];
  [" _____ "; "|2    |"; "|  v  |"; "|     |"; "|  v  |"; "|____Z|"];
  [" _____ "; "|3    |"; "| v v |"; "|     |"; "|  v  |"; "|____E|"];
  [" _____ "; "|4    |"; "| v v |"; "|     |"; "| v v |"; "|____h|"];
  [" _____ "; "|5    |"; "| v v |"; "|  v  |"; "| v v |"; "|____S|"];
  [" _____ "; "|6    |"; "| v v |"; "| v v |"; "| v v |"; "|____9|"];
  [" _____ "; "|7    |"; "| v v |"; "|v v v|"; "| v v |"; "|____L|"];
  [" _____ "; "|8    |"; "|v v v|"; "| v v |"; "|v v v|"; "|____8|"];
  [" _____ "; "|9    |"; "|v v v|"; "|v v v|"; "|v v v|"; "|____6|"];
  [" _____ "; "|10 v |"; "|v v v|"; "|v v v|"; "|v v v|"; "|___0I|"];
  [" _____ "; "|J  ww|"; "|   {)|"; "|(v)% |"; "| v % |"; "|__%%[|"];
  [" _____ "; "|Q  ww|"; "|   {(|"; "|(v)%%|"; "| v%%%|"; "|_%%%O|"];
  [" _____ "; "|K  WW|"; "|   {)|"; "|(v)%%|"; "| v%%%|"; "|_%%%>|"]
]

let diamonds = [
  [" _____ "; "|A ^  |"; "| / \\ |"; "| \\ / |"; "|  .  |"; "|____V|"];
  [" _____ "; "|2    |"; "|  o  |"; "|     |"; "|  o  |"; "|____Z|"];
  [" _____ "; "|3    |"; "| o o |"; "|     |"; "|  o  |"; "|____E|"];
  [" _____ "; "|4    |"; "| o o |"; "|     |"; "| o o |"; "|____h|"];
  [" _____ "; "|5    |"; "| o o |"; "|  o  |"; "| o o |"; "|____S|"];
  [" _____ "; "|6    |"; "| o o |"; "| o o |"; "| o o |"; "|____9|"];
  [" _____ "; "|7    |"; "| o o |"; "|o o o|"; "| o o |"; "|____L|"];
  [" _____ "; "|8    |"; "|o o o|"; "| o o |"; "|o o o|"; "|____8|"];
  [" _____ "; "|9    |"; "|o o o|"; "|o o o|"; "|o o o|"; "|____6|"];
  [" _____ "; "|10 o |"; "|o o o|"; "|o o o|"; "|o o o|"; "|___0I|"];
  [" _____ "; "|J  ww|"; "| /\\{)|"; "| \\/% |"; "|   % |"; "|__%%[|"];
  [" _____ "; "|Q  ww|"; "| /\\{(|"; "| \\/%%|"; "|  %%%|"; "|_%%%O|"];
  [" _____ "; "|K  WW|"; "| /\\{)|"; "| \\/%%|"; "|  %%%|"; "|_%%%>|"]
]

let face_down = [
  " _____ "; "|\\ ~ /|"; "|}}:{{|"; "|}}:{{|"; "|}}:{{|"; "|/_~_\\|"
]

(** [card_to_index_suit c] returns a tuple of list index and suit depending on
    the value of card [c]. *)
let card_to_index_suit (card: t) =
  let (_, c, s) = card in
  match c with
  | Jack -> 10, s
  | Queen -> 11, s
  | King -> 12, s
  | Ace -> 0, s
  | Int i -> (i-1), s

(** [to_string_lst i s] returns a string list depending on index [i] and
    suit [s]. *)
let to_string_lst (i, s) =
  match s with
  | Diamonds -> List.nth diamonds i
  | Spades -> List.nth spades i
  | Hearts -> List.nth hearts i
  | Clubs -> List.nth clubs i

(** [card_string_lst_to_string a l] turns a list of string lists that
    represent cards into a full string that is the ASCII art for list [l],
    using accumulator [a]. *)
let rec card_string_lst_to_string acc (lst : (string list) list) =
  match lst with
  | h::t ->
    if List.length h > 0 then
      let append_lst = List.map List.hd lst in
      let append = List.fold_left (^) "" append_lst in
      let remaining = List.map List.tl lst in
      card_string_lst_to_string (acc ^ append ^ "\n") remaining 
    else acc
  | _ -> acc

let card_facedown_to_ascii_string (hand : t list) = 
  let top = List.hd hand in
  let (i,s) = card_to_index_suit top in
  let cards = (to_string_lst (i,s))::[face_down] in
  card_string_lst_to_string "" cards

let cards_to_ascii_string (cards : t list) =
  let cards_index_suits = List.map card_to_index_suit cards in
  let cards_string_lsts = List.map to_string_lst cards_index_suits in
  card_string_lst_to_string "" cards_string_lsts

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

