module type Card = sig
  type card
  val to_string : card -> string
end

module Card = struct
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
end