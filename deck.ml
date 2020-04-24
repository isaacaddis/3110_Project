open Card

type deck = card list

let shuffle_deck =
  let rec make_deck n acc =
    match n with
    | n when n < 52 -> 
      make_deck (n+1) ((make_card n) :: acc)
    | _ -> acc
  and shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
  in shuffle (make_deck 1 [])

let draw_card d = 
  match d with
  | [] -> failwith "there are no more cards to deal"
  | x :: xs -> (x, xs)
