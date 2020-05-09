open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson

let login name =
  Client.post ~body:(Cohttp_lwt.Body.of_string name) 
    (Uri.of_string "http://localhost:8000/login") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n Body: \n" (String.length body);
  body

let play session_id =
  Client.post ~body:(Cohttp_lwt.Body.of_string session_id) 
    (Uri.of_string "http://localhost:8000/play") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let minisleep (sec: float) =
  Unix.sleepf sec

let rec main session_id () = 
  minisleep 5.;
  let response = Lwt_main.run (play session_id) in
  print_endline ("Received response\n" ^ response);
  main session_id ()

let () =
  print_endline "Welcome to blackjack. There's an open seat on the table";
  print_endline "Your name is: ";
  let name = read_line () in 
  let session_id = Lwt_main.run (login name) in
  main session_id () 
