open Lwt
open Cohttp
open Cohttp_lwt_unix
open State
open Yojson
open Yojson.Basic.Util

type user_data = { name : string; bet: int;  state : State.t }

type v = ((string, user_data) Hashtbl.t)

let connected_users : v = Hashtbl.create 3

type db = { mutable connected_users: v }
let db = ref { connected_users = connected_users};;

(** [check_user_connected id] is [Some x] if [id] is connected *)
let check_user_connected tbl (session_id: string) : user_data option=
  Hashtbl.find_opt tbl session_id

(** [is_full t] is [false] when there are 2 players or less at the table [t],
    [true] otherwise. *)
let is_full tbl =
  Hashtbl.length tbl >= 3

(** [add_player s n] adds an entry to the database. *)
let add_player (session_id: string) (bet: int) (name: string) tbl = 
  Hashtbl.add tbl session_id { name = name; bet = bet; state = init_state };
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

type start_conditions = Start| Wait

let check_start_condition tbl = 
  if is_full tbl then Start else Wait

(** [handle_play s] handles the main game loop response for session_id [s]
    Requires: [s] is a session_id that is in [db]
    Effects: prints to console *)
let handle_play ( session_id: string) = 
  try
    let state = Hashtbl.find (!db.connected_users) (session_id) in
    match state with
    | res -> 
          let name = res.name in
          let bet = res.bet in
          let state  = res.state in
          match check_start_condition (!db.connected_users) with
          | Start -> Printf.sprintf "Game running."
          | Wait -> Printf.sprintf "Need 3 players to start game."
  with Not_found -> Printf.sprintf "Unauthorized access."
  
  
let parse_login_json (j: Yojson.Basic.t) : (string * int) =
  let name = j |> member "name" |> to_string in
  let bet = j |> member "bet" |> to_int in
  (name, bet)

let parse_play_json (j: Yojson.Basic.t) : string =
  let session_id = j |> member "session_id" |> to_string in
  session_id

let handle_response (uri: string) (body_string : string) =  
    let db = !db.connected_users in
    let json = Yojson.Basic.from_string body_string in
    try
      if contains uri "/login" then
        begin
        let (name, bet) = parse_login_json json in
        match add_user db name bet with
        | Success (id,tbl) ->
            Printf.sprintf "%s" id
        | Failure -> Printf.sprintf "The table is full."
        end
      else 
      handle_play (parse_play_json json)
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
