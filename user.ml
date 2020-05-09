open Lwt
open Cohttp
open Cohttp_lwt_unix

let body name =
  Client.post ~body:(Cohttp_lwt.Body.of_string name) 
    (Uri.of_string "http://localhost:8000") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let minisleep (sec: float) =
  Unix.sleepf sec

let rec main name () = 
  minisleep 5.;
  let body' = Lwt_main.run (body name) in
  print_endline ("Received body\n" ^ body');
  main body' ()

let () =
  print_endline "Welcome to blackjack. There's an open seat on the table";
  print_endline "Your name is: ";
  let name = read_line () in 
  main name () 
