open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util

type state = { session_id: string ref }

let state_ref = { session_id = ref "" }

let json_from_string (str: string) : Yojson.Basic.t = 
  Yojson.Basic.from_string str

let construct_login_json name bet = 
  "{ \"name\" : \"" ^ 
  name ^ 
  "\", \"bet\" : " ^ 
  (string_of_int bet) ^ " }"

let construct_play_json (session_id: string) =
  "{ \"session_id\" : \"" ^
  session_id ^ 
  "\" }"

let login name bet =
  Client.post ~body:(Cohttp_lwt.Body.of_string (construct_login_json name bet))
    (Uri.of_string "http://localhost:8000/login") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "\nResponse code: %d\n" code;
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

let play session_id =
  Client.post 
    ~body:(Cohttp_lwt.Body.of_string (construct_play_json (session_id)) )
    (Uri.of_string "http://localhost:8000/play") >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

let minisleep (sec: float) =
  Unix.sleepf sec

let contains s1 s2 = 
  let re = Str.regexp_string s2
  in
    try if Str.search_forward re s1 0 > -1 then true else false
    with Not_found -> false

let parse_play_json (json_string:string) : (bool * string) = 
  if json_string = "Unauthorized access." then raise Not_found else
  let json = json_from_string json_string in 
  let your_turn = json |> member "your_turn" |> to_bool in
  let status = json |> member "status" |> to_string in 
  (your_turn, status)

let rec main session_id () = 
  minisleep 1.;
  let response = Lwt_main.run (play session_id) in
  begin
    match parse_play_json response with
    | exception Not_found -> print_endline "You cannot join a full lobby"
    | (false, _) -> print_endline "Game not started or it's not your turn"
    | (true, status) -> print_endline status
  end;
  main session_id ()

let () =
  print_endline "Welcome to blackjack. There's an open seat on the table";
  print_endline "Your name is: ";
  let name = read_line () in 
  print_endline "Bet: ";
  let bet = read_line () |> int_of_string in
  let session_id =  Lwt_main.run (login name bet) in
  (state_ref.session_id) := session_id;
  let session_id' = 
    begin
      match session_id with
      | "The table is full." -> failwith "You cannot join a full game."
      | x -> x
    end
  in
  print_endline ("Joined with session ID: " ^ session_id');
  main (session_id') () 
