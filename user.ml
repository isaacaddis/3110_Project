open Lwt
open Cohttp
open Cohttp_lwt_unix

let body name =
  Client.post ~body:(Cohttp_lwt.Body.of_string name) (Uri.of_string "http://localhost:8000") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  print_endline "Welcome to blackjack. There's an open seat on the table";
  print_endline "Your name is: ";
  let name = read_line () in 
  let body' = Lwt_main.run (body name) in
  print_endline ("Received body\n" ^ body')
(**
(** [server_err s b] prints an error response with status code [s] 
    and body [b].
    Returns: ()
    Effects: Prints to console *)
let server_err status : unit = print_endline ("Err " ^ status ^ ":")


(** [ok_response_action b] handles a server response body [b]
    Returns: ()
    Effects: Prints to console *)
let ok_response_action (body: Cohttp_lwt.Body.t) : unit= 
  failwith "kms"
(**  print_endline (Cohttp_lwt.Body.to_string body)*)

let body =
  Client.post ~body:(Cohttp_lwt.Body.of_string "test")
        (Uri.of_string "https://localhost:3000)") >>= 
        fun (response, body) ->
          match (response, body) with
                | (_, body) -> Cohttp_lwt.Body.to_string body
                | _ -> failwith "err"
  (**
  Client.post
    ~body:(Cohttp_lwt.Body.of_string "test")
    (Uri.of_string "http://localhost:3000")
  >>= fun (response, body) ->
    match response with 
    | { Cohttp.Response.status = `OK; _ } ->
        failwith "kill yourself";
  (**      ok_response_action body *)
    | { Cohttp.Response.status; _ } -> failwith "unimplemented";
        (**
        response |> 
        Response.status |>
        Code.code_of_status |>
        string_of_int |>
        server_err
        *)
    body 
    *)


let () =
  print_endline "Welcome to blackjack. You take a seat at the table.";
  print_endline "Enter your name: ";
  let name = read_line () in
  let body' = Lwt_main.run (body) in
  print_endline ("Received body\n" ^ body')
*)
