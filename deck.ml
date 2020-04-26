open Card

type deck = card list
type res = { cards: card list; deck: deck }

let cards r =
  r.cards

let cards_to_string (r: res) =
  let cards = cards r in
  let rec concat (c: card list) (acc: string list) =
    match c with
    | [] -> acc
    | x :: xs -> concat xs ((to_string (x)) :: acc)
  in concat cards []

let deck r = 
  r.deck 

(** Requires: n is in [0..51] **)
let match_suit n = 
  match n with
  | n when n < 13 -> 1
  | n when n < 26 -> 2
  | n when n < 39 -> 3
  | n when n < 52 -> 4
  | _ -> failwith "n must be in [0...51]"

let shuffle_deck =
  let rec make_deck n acc =
    match n with
    | n when n < 52 -> 
      let s = match_suit n in 
      make_deck (n+1) ((make_card (n mod 13) s ) :: acc)
    | _ -> acc
  and shuffle d =
    Random.self_init ();
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
  in shuffle (make_deck 1 [])

let draw_card d = 
  match d with
  | [] -> failwith "there are no more cards to deal"
  | x :: xs -> { cards = [x]; deck = xs }

let draw_two_cards d =
  match d with
  | x :: y :: xs -> { cards = x :: y :: []; deck = xs }
  | _ -> failwith "there are no more cards to deal"