
open Utils

(* The Chat application needs it's own user because the original one is trapped
   within the [Chat.Make] functor. *)
module User = struct
  type t = {
    id : string;
    color : string;
  } deriving (Json)
  let id { id } = id
  let compare = on id compare
  let hash = Hashtbl.hash -| id
  let equal = on id (=)
  let parameter = flip Eliom_parameters.caml Json.t<t>
end
module User_set = struct
  include Set.Make (User)
  let to_string =
    String.concat ", " -| List.map User.id -| elements
end

module Conversation = struct 

  type message = {
    msg_author : User.t;
    msg_content : string;
  } deriving (Json)

  type t = {
    id : int;
    bus : message Eliom_bus.t;
    users : User_set.t;
  }

  let create =
    let counter = ref 0 in
    fun bus users ->
      let id = incr counter; !counter in
      { id; bus; users }

  let users { users } = users
  let compare = on users User_set.compare

end

(* The application-wide events *)
type event =
  | Append_conversation of Conversation.t
  | Remove_conversation of Conversation.t

let event_to_string = function
  | Append_conversation conv ->
      Printf.sprintf "Append_conversation %d" conv.Conversation.id
  | Remove_conversation conv ->
      Printf.sprintf "Remove_conversation %d" conv.Conversation.id

(* HTML generation functions shared between client and server *)

let user_span ?self user =
  let open HTML5.M in
  let self_class =
    let self_class user' = if User.equal user user' then ["self"] else [] in
    get_option ~default:[] **> map_option self_class self
  in
  span ~a:([a_class ("user_name" :: self_class); a_style ("background-color: "^user.User.color)]) [
    pcdata user.User.id
  ]

let create_user_li ?self =
  HTML5.M.li -| singleton -| user_span ?self

(* Translate changes in a users signal into events removing/adding users. *)
let user_added_event, user_removed_event =
  let diff_users_event order =
    let get_diff before_after =
      let res = uncurry User_set.diff (order before_after) in
      match User_set.cardinal res with
          0 -> None
        | 1 -> Some (User_set.choose res)
        | _ -> failwith "users signal should only be modified by one user each"
    in
    Lwt_react.(E.fmap get_diff -| S.diff pair)
  in
  diff_users_event identity, diff_users_event flip_pair

