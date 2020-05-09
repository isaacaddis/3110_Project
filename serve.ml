open Lwt
open Cohttp
open Cohttp_lwt_unix
open State

type user_data = { name : string; state : State.t }

type v = ((string, user_data) Hashtbl.t)

let connected_users : v = Hashtbl.create 3

type db = { mutable connected_users: v }
let db = ref { connected_users = connected_users};;

(** [check_user_connected id] is [Some x] if [id] is connected *)
let check_user_connected tbl (session_id: string) =
  Hashtbl.find_opt tbl session_id

(** [is_full t] is [false] when there are 2 players or less at the table [t],
    [true] otherwise. *)
let is_full tbl =
  Hashtbl.length tbl >= 3

let add_player (session_id: string) (name: string) tbl = 
  Hashtbl.add tbl session_id { name = name; state = init_state };
  tbl

type status = Success of (string, user_data) Hashtbl.t | Failure

(** [add_user t n] is a dictionary [d] of player [n] in table [t] *)
let add_user tbl name =
  let session_id = Random.int 100000 |> string_of_int in
  if is_full tbl then Failure else
  Success (add_player session_id name tbl)

let register_user connected_users (name : string) = 
  add_user connected_users name

(** [contains s1 s2] is [true] when s2 is a substring of s1, 
    [false] otherwise.*) 
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try if Str.search_forward re s1 0 > -1 then true else false
        with Not_found -> false

(** [handle_play tbl s] handles the main game loop response for session_id [s]
    Requires: [s] is a session_id that is in [tbl]
    Effects: prints to console *)
let handle_play tbl session_id = 
  Printf.sprintf "Game action: %s" session_id
  
let handle_response (uri: string) (body_string : string) =  
    let db = !db.connected_users in
    try
      if contains "/login" uri then
        begin
        let at_table = (fun h -> Hashtbl.fold (fun k v acc -> k :: acc) h []
         |> String.concat ", ") in 
        match add_user db body_string with
        | Success tbl ->
            Printf.sprintf "%s has joined successfully. At table: %s. "
            body_string (at_table tbl)
        | Failure -> Printf.sprintf "The table is full."
        end
      else handle_play db body_string
    with Not_found -> handle_play db body_string



let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (handle_response uri body))
    >>= ( fun body -> Server.respond_string ~status:`OK ~body:(body ^ uri) ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = 
  Random.self_init();
  Lwt_main.run server
