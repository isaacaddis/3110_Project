open Player
open State

type pts = int
type condition = Bust | Natural | Int of pts
type win_condition = Win | Draw | Loss | Stop of pts | Next of pts
type win = win_condition * win_condition

let get_player_condition player =
  (* let hand = hand player in *)
  match points player with
  | 21 -> Natural
  | n when n > 21 -> Bust
  | n -> Int n

type move = Stand | Hit | Quit

let parse = function
  | "stand" -> Stand
  | "hit" -> Hit
  | "quit" -> Quit
  | _ -> failwith "unrecognized"

let step_st st move =
  match move with
  | Stand -> st
  | Quit -> print_endline "Exiting"; exit 0
  | Hit -> step st

let check_st st =
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
      begin match d_cond with
      | Natural -> (Loss, Win)
      | Bust -> (Win, Loss)
      | Int y -> if y >= 17 then (Next x, Stop y) else (Next x, Next y)
      end
