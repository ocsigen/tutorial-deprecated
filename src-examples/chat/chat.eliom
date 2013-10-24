
{shared{
  open Eliom_content
  open Utils
  open Shared
}}

let css_files = [["chat.css"]]

(* Services *)

let create_dialog_service =
  Eliom_service.Http.post_coservice' ~post_params:(User.parameter "other") ()
let cancel_dialog_service =
  Eliom_service.Http.post_coservice' ~post_params:(Eliom_parameter.int "conversation_id") ()

(* User info *)

type user_info = {
  events : chat_event React.E.t;
  send : chat_event -> unit;
  mutable conversations : conversation list;
}

let get_user_info, create_user_info, drop_user_info, get_all_user_infos =
  let module User_map = Map.Make (User) in
  let user_infos = ref User_map.empty in
  let get_user_info user =
    User_map.find user !user_infos
  in
  let create_user_info user =
    assert (not **> User_map.mem user !user_infos);
    let user_info =
      let events, send = React.E.create () in
      { events; send; conversations = [] }
    in
    user_infos := User_map.add user user_info !user_infos
  in
  let drop_user_info user =
    user_infos := User_map.remove user !user_infos
  in
  let get_all_user_infos () =
    User_map.bindings !user_infos
  in
  get_user_info, create_user_info, drop_user_info, get_all_user_infos

let users_by_conversation conversation_p =
  User_set.of_elements **>
    List.map_filter
      (fun (user, user_info) ->
         if List.exists conversation_p user_info.conversations then
           Some user
         else
           None)
      (get_all_user_infos ())

(* Dialogs *)

let create_dialog user users =
  let conversation =
    let bus = Eliom_bus.create Json.t<conversation_event> in
    create_conversation bus users
  in
  let send_to_user user =
    debug "Send conversation %d to %s" conversation.id user.User.name;
    let user_info = get_user_info user in
    user_info.conversations <- conversation :: user_info.conversations;
    user_info.send **> Append_conversation (conversation, users);
  in
  User_set.iter send_to_user users

let cancel_dialog user conversation_id =
  List.iter
    (fun (user, user_info) ->
       try
         let conversation =
           List.find (fun { id } -> id = conversation_id) user_info.conversations
         in
         debug "Remove_conversation %d of user %s" conversation_id user.User.name;
         user_info.conversations <- List.filter (fun { id } -> id <> conversation_id) user_info.conversations;
         user_info.send **> Remove_conversation conversation
       with Not_found -> ())
    (get_all_user_infos ())


(* Connection callbacks *)

let on_create_first_connection user =
  debug "Create user info";
  create_user_info user

let on_drop_last_connection user =
  debug "Drop user info";
  let user_info = get_user_info user in
  List.iter
    (fun conversation ->
       cancel_dialog user conversation.id)
    user_info.conversations;
  drop_user_info user

(* The chat element *)

let render user =
  fun () () ->
    debug "render for %s" user.User.name;
    User_management.init_connection user;
    let user_info = get_user_info user in
    let conversations =
      List.map
        (fun c -> c, users_by_conversation (fun {id} -> c.id = id))
        user_info.conversations
    in
    let onload = {{
      Client.onload %user %(Eliom_react.Down.of_react user_info.events) %conversations
        %(Eliom_react.S.Down.of_react User_management.users_signal)
        %Widgets.users_id %Widgets.conversations_id
        %create_dialog_service %cancel_dialog_service
    }} in
    Lwt.return **>
      Widgets.main user onload

(* Handlers *)

let create_dialog_handler user =
  fun () other ->
    let users = User_set.(add user (add other empty)) in
    debug "Create dialog between %s" (User_set.to_string users);
    create_dialog user users;
    Lwt.return ()

let cancel_dialog_handler user =
  fun () conversation_id ->
    debug "Cancel dialog %d" conversation_id;
    cancel_dialog user conversation_id;
    Lwt.return ()

(* Registration *)

let register () =
  Eliom_registration.Action.register ~service:create_dialog_service
    (User_management.connected_action create_dialog_handler);
  Eliom_registration.Action.register ~service:cancel_dialog_service
    (User_management.connected_action cancel_dialog_handler)
