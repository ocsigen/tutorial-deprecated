
open Utils
open Eliom_content

module User = struct
  type t = {
    id : int;
    name : string;
    color : string;
  } deriving (Json)
  let compare = on (fun { id } -> id) compare
  let equal = on (fun { id } -> id) (=)
  let hash { id } = id
  let create =
    let counter = ref 0 in
    fun name ->
      let id = incr counter; !counter in
      let color =
        Printf.sprintf "#%02x%02x%02x"
          (56+Random.int 200) (56+Random.int 200) (56+Random.int 200)
      in
      { id; name; color }
  let parameter = flip Eliom_parameter.caml Json.t<t>
end

module User_set = struct
  include Set.Make (User)
  let to_string =
    String.concat ", " -| List.map (fun { User.name } -> name) -| elements
  let of_elements elts =
    List.fold_right add elts empty
end

let participants_data =
  Html5.Custom_data.create_json ~name:"participants"
    Json.t<User.t list>

type message = {
  author : User.t;
  content : string;
} deriving (Json)

type conversation_event =
  | Message of message
  deriving (Json)

type conversation = {
  id : int;
  bus : conversation_event Eliom_bus.t;
  users : User_set.t;
}

let create_conversation =
  let counter = ref 0 in
  fun bus users ->
    let id = incr counter; !counter in
    debug "create_conversation %d" id;
    { id; bus; users }

let id_of_conversation { id } = "conversation_"^string_of_int id

type chat_event =
  | Append_conversation of conversation * User_set.t
  | Remove_conversation of conversation


