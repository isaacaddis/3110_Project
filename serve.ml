open Lwt
open Cohttp
open Cohttp_lwt_unix
open State

type user_data = { name : string; state : State.t }

let connected_users : ((string, user_data) Hashtbl.t) = Hashtbl.create 3

(** [check_user_connected id] is [Some x] if [id] is connected *)
let check_user_connected tbl (session_id: string) =
  Hashtbl.find_opt tbl session_id

(** [is_full t] is [false] when there are 2 players or less at the table [t],
    [true] otherwise. *)
let is_full tbl =
  Hashtbl.length tbl > 3

let add_player (session_id: string) (name: string) tbl = 
  Hashtbl.add tbl session_id { name = name; state = init_state };
  tbl


type status = Success of (string, user_data) Hashtbl.t | Failure

(** [add_user t n] is a dictionary [d] of player [n] in table [t] *)
let add_user tbl name =
  let session_id = Random.int 100000 |> string_of_int in
  if is_full tbl then Failure else
  Success (add_player session_id name tbl)

let handle_response body_string = 
    let name = body_string in 
    let res = add_user connected_users name in
    match res with
    | Success tbl -> Printf.sprintf "successful"
    | Failure -> Printf.sprintf "unimplemented"


let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (handle_response body))
    >>= ( fun body -> Server.respond_string ~status:`OK ~body:body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = 
  Random.self_init();
  Lwt_main.run server
