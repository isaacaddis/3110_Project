open OUnit2
open Card
open Deck
open Player
open State

(** Test Plan: We tested all of the methods that were not based on direct inputs
    from the user. The methods that we play tested were in main.ml, deck.ml,
    and state.ml, since drawing and playing isn't standardized and the
    cards you draw would be different every time. We also manually tested the
    methods that would read/write information to/from a JSON. To test everything
    else in this, we wrote general cases and edge cases for methods. *)

let eq_test name a b = (name >:: fun _ -> assert_equal a b)

let card_tests = [
  eq_test "to_string jack" (make_card 10 1 |> to_string) "J of Diamonds";
  eq_test "to_string ace" (make_card 1 3 |> to_string) "A of Hearts";
  eq_test "to_string eight" (make_card 8 2 |> to_string) "8 of Spades";
  eq_test "get_val king" (make_card 12 1 |> get_val) 10;
  eq_test "get_val eight" (make_card 8 2 |> get_val) 8;
  eq_test "get_val ace" (make_card 1 1 |> get_val) 1;
]

let deck_tests = 
  let shuffled = shuffle_deck () in
  let drew_two = draw_two_cards shuffled in
  let remaining_cards = deck drew_two in
  let cards_drawn = cards drew_two in
[
  (** Make test cases for these, or maybe combine this stuff with other stuff
      for test cases. *)
]

let player_tests = 
  let cards = (make_card 1 1) :: (make_card 1 2) :: (make_card 9 1) :: [] in
  let player = make_player cards 500 in
  let cards2 = (make_card 5 3) :: (make_card 12 2) :: [] in
  let player2 = make_player cards2 1000 in
  [
    eq_test "Player1 money $500 equals $500" (money player) 500;
    eq_test "Player2 money $1000 equals $1000" (money player2) 1000;
    eq_test "Player1 hand equals its card list" (hand player) cards;
    eq_test "Player2 hand equals its card list" (hand player2) cards2;
    eq_test "Player1 top card equals its first card string" (top_card player) 
      (to_string (make_card 1 1));
    eq_test "Player2 top card equals its first card string" (top_card player2) 
      (to_string (make_card 5 3));
    eq_test "Player with 5 and king have points of 15" (points player2) 15;
    eq_test "Player with two aces and nine have 21 points" (points player) 21;
    eq_test "Player with three aces and a nine have 12 points" 
      (points (make_player ((make_card 1 3)::cards) 500)) 12;
    eq_test "Player points bust, four aces, a nine, and a king equals 22" 
      (points (make_player ((make_card 1 1)::(make_card 12 1)::cards) 500)) 22;
    eq_test "Player hand as string equals its cards converted to string"
      (hand_as_string player) "A of Diamonds, A of Spades, 9 of Diamonds";
    eq_test "Player2 hand as string equals its cards converted to string"
      (hand_as_string player2) "5 of Hearts, K of Spades";
  ]

let state_tests = 
  let st = init_state 500 in
  let p = player st in
  let d = dealer st in
  let blackjack = step_round st 1 300 in
  let blackjack2 = step_round st 1 500 in
  let win = step_round st 2 300 in
  let win2 = step_round st 2 500 in
  let loss = step_round st 3 500 in
  let loss2 = step_round st 3 100 in
  let draw = step_round st 4 300 in
  let draw2 = step_round st 4 500 in
  let dwin = step_round st 5 250 in
  let dwin2 = step_round st 5 100 in
  let dloss = step_round st 6 250 in
  let dloss2 = step_round st 6 100 in
  let blackjack3 = step_round blackjack2 1 500 in
  [
    eq_test "Player who just starts in st with 500 in stats file has $500" 
      (money p) 500;
    eq_test "Dealer starts out with $50000" (money d) 50000;
    eq_test "Player who won blackjack with bet $300 wins $450, has $950"
      (money (player blackjack)) 950;
    eq_test "Dealer who lost blackjack with bet $300 loses $450, has $49550"
      (money (dealer blackjack)) 49550;
    eq_test "Player who won blackjack with bet $500 wins $750, has $1250"
      (money (player blackjack2)) 1250;
    eq_test "Dealer who lost blackjack with bet $500 loses $750, has $49250"
      (money (dealer blackjack2)) 49250;
    eq_test "Player who won regularly with bet $300 wins $300, has $800"
      (money (player win)) 800;
    eq_test "Dealer who lost regularly with bet $300 loses $300, has $49700"
      (money (dealer win)) 49700;
    eq_test "Player who won regularly with bet $500 wins $500, has $1000"
      (money (player win2)) 1000;
    eq_test "Dealer who lost regularly with bet $500 loses $500, has $49500"
      (money (dealer win2)) 49500;
    eq_test "Player who lost with bet $500 loses $500, has $0"
      (money (player loss)) 0;
    eq_test "Dealer who won with bet $500 wins $500, has $50500"
      (money (dealer loss)) 50500;
    eq_test "Player who lost with bet $100 loses $100, has $400"
      (money (player loss2)) 400;
    eq_test "Dealer who won with bet $100 wins $100, has $50100"
      (money (dealer loss2)) 50100;
    eq_test "Player who tied with bet $300 gains nothing, has $500"
      (money (player draw)) 500;
    eq_test "Dealer who tied with bet $300 gains nothing, has $50000"
      (money (dealer draw)) 50000;
    eq_test "Player who tied with bet $500 gains nothing, has $500"
      (money (player draw2)) 500;
    eq_test "Dealer who tied with bet $500 gains nothing, has $50000"
      (money (dealer draw2)) 50000;
    eq_test "Player who won with double $250 gains $500, has $1000"
      (money (player dwin)) 1000;
    eq_test "Dealer who lost with double $250 loses $500, has $49500"
      (money (dealer dwin)) 49500;
    eq_test "Player who won with double $100 gains $200, has $700"
      (money (player dwin2)) 700;
    eq_test "Dealer who lost with double $100 loses $200, has $49800"
      (money (dealer dwin2)) 49800;
    eq_test "Player who lost with double $250 loses $500, has $0"
      (money (player dloss)) 0;
    eq_test "Dealer who won with double $250 wins $500, has $50500"
      (money (dealer dloss)) 50500;
    eq_test "Player who lost with double $100 loses $200, has $300"
      (money (player dloss2)) 300;
    eq_test "Dealer who lost with double $100 wins $200, has $50200"
      (money (dealer dloss2)) 50200;
    eq_test ("Test continuity, player wins blackjack with bet $500 gains " ^ 
      "$750 again, has $2000") (money (player blackjack3)) 2000;
    eq_test ("Test continuity, dealer loses blackjack with bet $500 loses " ^ 
      "$750 again, has $48500") (money (dealer blackjack3)) 48500;
]

let suite =  
  "test suite for blackjack" >::: List.flatten [
    card_tests;
    deck_tests;
    player_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite