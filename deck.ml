open Card

type deck = 

(** The type of values representing an exit. *)
type exit = {
  name: exit_name;
  exit_id: room_id
}

