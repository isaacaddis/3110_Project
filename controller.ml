open Player
open State

type pts = int
type condition = Bust | Natural | Int of pts
type win_condition = Win | Draw | Loss | Next
type win = win_condition * win_condition

let get_player_condition player =
  (* let hand = hand player in *)
  match points player with
  | 21 -> Natural
  | n when n > 21 -> Bust
  | n -> Int n

(** [check_st s] checks if either the player or dealer has one yet. **)
let check_st initial_run st =
  let player = player st in
  let dealer = dealer st in
  let p_cond = get_player_condition player in
  let d_cond = get_player_condition dealer in
  match p_cond with
  | Bust -> (Loss, Win)
  | Natural ->
      begin
        match d_cond with
        | Natural -> (Draw, Draw)
        | _ -> (Win, Loss)
      end
  | Int x ->
      if initial_run = false then
        begin match d_cond with
        | Natural -> (Loss, Win)
        | Bust -> (Win, Loss)
        | Int y ->
          if y >= 17 then
            begin
              if x > y then (Win, Loss) else (Loss, Win)
            end
          else (Next, Next)
        end
      else (Next, Next)
