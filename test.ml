open OUnit2
open Card
open Deck
open Player
open State

let eq_test name a b = (name >:: fun _ -> assert_equal a b)

let card_tests = [
  eq_test "to_string jack" (make_card 10 1 |> to_string) "J of Diamonds";
  eq_test "to_string ace" (make_card 1 3 |> to_string) "A of Hearts";
  eq_test "get_val king" (make_card 12 1 |> get_val) 10;
  eq_test "get_val eight" (make_card 8 2 |> get_val) 8;
  eq_test "get_val ace" (make_card 1 1 |> get_val) 1;
]

let deck_tests = []

let player_tests = 
  let cards = (make_card 1 1) :: (make_card 1 2) :: (make_card 9 1) :: [] in
  let player = make_player cards 500 in
  [
    eq_test "Player.points two aces and a nine equals 21" (points player) 21;
    eq_test "Player.points three aces and a nine equals 12 " 
      (points (make_player ((make_card 1 3)::cards) 500)) 12;
    eq_test "Player.points bust, four aces, a nine, and a queen equals 22" 
      (points (make_player ((make_card 1 1)::(make_card 12 1)::cards) 500)) 22;
  ]

let state_tests = []

let suite =  
  "test suite for blackjack" >::: List.flatten [
    card_tests;
    deck_tests;
    player_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite