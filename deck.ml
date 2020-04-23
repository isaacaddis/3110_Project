open Card

type deck = card list

let init () =
  let rec helper counter acc =
    if counter > 51 then acc
    else helper (counter + 1) (make_card counter)::acc
  in
  helper 0 []