open Card

module Player = struct
  type player = { hand: string list }

  let init_player h =
    { hand = h }

  let rec pts = function
    | [] -> 0
    | x :: xs -> (get_val x) + (pts xs)

  let hand p =
    List.map (fun c -> to_string c) 
end 
