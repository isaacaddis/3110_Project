open OUnit2
open Card
open Deck
open Player
open State

let card_tests = [
  let jack = make_card 10 1 in
  "to_string" >:: fun _ -> assert_equal (to_string jack) "J of Diamonds"
]

let deck_tests = []

let player_tests = []

let state_tests = []

let suite =  
  "test suite for blackjack" >::: List.flatten [
    card_tests;
    deck_tests;
    player_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite