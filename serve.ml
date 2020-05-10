open Lwt
open Cohttp
open Cohttp_lwt_unix
open State
open Yojson
open Yojson.Basic.Util
open Controller
open Player
open Deck
open State

type user_data = { name : string; bet: int;  state : State.t }

type v = ((string, user_data) Hashtbl.t)

let connected_users : v = Hashtbl.create 3

(* [db] is the state of the game *)
type db = { dealer: Player.t ref; turn: int ref; connected_users: v ref }

let d = shuffle_deck ()
let d' = draw_two_cards d
let db = { dealer = ref (make_player (cards d') 50000); turn = ref 0;
  connected_users = ref connected_users}

type keys = { session_ids: string list ref }
let keys = { session_ids = ref [] }

let connected = ref 0

let connect_user () = 
  connected := (!connected) + 1

let disconnect_user () = 
  connected := (!connected) - 1

let next_turn () =
  (db.turn) := (!(db.turn) + 1) mod (!connected)

let next_turn_and_leave () =
  let ids = !(keys.session_ids) in
  let session_id = List.nth ids !(db.turn) in
  disconnect_user ();
  if !connected > 0 then
    begin
    next_turn ();
    (keys.session_ids) := 
      List.filter (fun e -> e != session_id) !(keys.session_ids)
    end
  else ()

(** [check_user_connected id] is [Some x] if [id] is connected *)
let check_user_connected tbl (session_id: string) : user_data option=
  Hashtbl.find_opt tbl session_id

(** [is_full t] is [false] when there are 2 players or less at the table [t],
    [true] otherwise. *)
let is_full tbl =
  Hashtbl.length tbl >= 3

(** [add_player s n] adds an entry to the database. *)
let add_player (session_id: string) (bet: int) (name: string) tbl = 
  connect_user ();
  Hashtbl.add tbl session_id { name = name; bet = bet; state = init_state bet};
  (session_id, tbl)

type status = Success of (string * ((string, user_data) Hashtbl.t)) | Failure

(** [add_user t n] is a dictionary [d] of player [n] in table [t] *)
let add_user tbl name bet =
  let session_id = Random.int 100000 |> string_of_int in
  if is_full tbl then Failure else
  Success (add_player session_id bet name tbl)

(** [contains s1 s2] is [true] when s2 is a substring of s1, 
    [false] otherwise.
    Requires: [s2.length] <= [s1.length] *) 
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try if Str.search_forward re s1 0 > -1 then true else false
        with Not_found -> false

type start_conditions = Start| Wait of int

let check_start_condition tbl = 
  if is_full tbl then Start else Wait (Hashtbl.length tbl)

(** [construct_play_json t s] constructs a JSON string of the format
    { 'your_turn': bool, 'status': 'status' }
    Requires: status is either "wait", "win", "tie", "loss", or "next" *)
let construct_play_json (your_turn:bool) (status: string) name bet =
  print_endline (string_of_int !connected);
  "{ \"your_turn\": " ^
  (string_of_bool your_turn) ^ 
  ", \"status\": \"" ^
  status ^
  "\", \"name\": \"" ^
  name ^
  "\", \"bet\": " ^
  (string_of_int bet) ^
  " }"

(** Effects: Updates the [db] with new paramters *)
let replace_db session_id name bet st' =
  let usrData' = 
    { name = name;
      bet = bet;
      state = st' }
  in
  Hashtbl.replace !(db.connected_users) session_id usrData'

(** [handle_next (session_id, action) runs the game action [action] for
    the user with session id [session_id], if it is their turn *)
let handle_next (tup:(string*string)) =
  let (session_id, action) = tup in
  try
    let usrData = Hashtbl.find !(db.connected_users) (session_id) in
    let name = usrData.name in
    let bet = usrData.bet in
    let st = usrData.state in
    let turn = !(db.turn) in
    let turn_session_id = List.nth !(keys.session_ids) (turn) in
    next_turn ();
    if turn_session_id = session_id then
        match Parser.parse action with
        | Hit -> 
            let st' = step st Hit in
            replace_db session_id name bet st';
            Printf.sprintf "Game state updated. Next turn."
        | Stand ->
            let st' = step st Stand in
            replace_db session_id name bet st';
            Printf.sprintf "Game state updated. Next turn."
        | _ -> exit 0
    else Printf.sprintf "Unauthorized access."
  with Not_found -> Printf.sprintf "Unauthorized access."

(** [handle_play s] handles the main game loop response for session_id [s]
    Requires: [s] is a session_id that is in [db]
    Effects: prints to console *)
let handle_play (session_id: string) = 
  try
    let state = Hashtbl.find !(db.connected_users) (session_id) in
    match state with
    | res -> 
          let dealer = !(db.dealer) in
          let name = res.name in
          let bet = res.bet in
          let st  = res.state in
          match check_start_condition !(db.connected_users) with
          | Start ->
            let turn = !(db.turn) in
            let turn_session_id = List.nth !(keys.session_ids) (turn) in
            if turn_session_id = session_id then
              begin
              let status = check_st' true (player st) dealer in
              match status with
              | (Blackjack, _ ) -> 
                  next_turn_and_leave ();
                  Printf.sprintf 
                    "%s" (construct_play_json true "win" name bet)
              | (Win, _ ) -> 
                  next_turn_and_leave ();
                  Printf.sprintf 
                    "%s" (construct_play_json true "win" name bet)
              | (Loss, _ ) -> 
                  next_turn_and_leave ();
                  Printf.sprintf 
                    "%s" (construct_play_json true "loss" name bet)
              | (Draw, _ ) -> 
                  next_turn_and_leave ();
                  Printf.sprintf 
                    "%s" (construct_play_json true "tie" name bet)
              | (Next, Next) ->
                  Printf.sprintf 
                    "%s" (construct_play_json true "next" name bet)
              | _ -> exit 0
              end
            else
              Printf.sprintf 
                "%s" (construct_play_json false "wait" name bet)
          | Wait _ -> 
            Printf.sprintf 
            "%s" (construct_play_json false "wait" name bet)
  with Not_found -> Printf.sprintf "Unauthorized access."
  
  
let parse_login_json (j: Yojson.Basic.t) : (string * int) =
  let name = j |> member "name" |> to_string in
  let bet = j |> member "bet" |> to_int in
  (name, bet)

let parse_play_json (j: Yojson.Basic.t) : string =
  let session_id = j |> member "session_id" |> to_string in
  session_id

let parse_next_json (j: Yojson.Basic.t) : (string * string) = 
  let session_id = j |> member "session_id" |> to_string in
  let action = j |> member "action" |> to_string in
  (session_id, action)

let handle_response (uri: string) (body_string : string) =  
    let db = !(db.connected_users) in
    let json = Yojson.Basic.from_string body_string in
    try
      if contains uri "/login" then
        begin
        let (name, bet) = parse_login_json json in
        match add_user db name bet with
        | Success (id,_) ->
            (keys.session_ids) := (id :: !(keys.session_ids));
            Printf.sprintf "%s" id
        | Failure -> Printf.sprintf "The table is full."
        end
      else
        try
          if contains uri "/play" then
            handle_play (parse_play_json json)
          else handle_next (parse_next_json json)
        with Not_found -> handle_next (parse_next_json json)
    with Not_found -> handle_play (parse_play_json json)

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (handle_response uri body))
    >>= ( fun body -> Server.respond_string ~status:`OK ~body:body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = 
  Random.self_init();
  Lwt_main.run server
