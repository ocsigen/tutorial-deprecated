
{shared{

  open Eliom_lib
  open Eliom_lib.Lwt_ops
  open Eliom_content
  open Lib

}}

{shared{

  module User = struct
    type t = {
      name : string;
      color : string;
    } deriving (Json)
    let create name =
      let color =
        Printf.sprintf "#%02x%02x%02x"
          (56+Random.int 200) (56+Random.int 200) (56+Random.int 200)
      in
      { name; color }
    let to_string { name } = name
    let group_name = to_string
    let compare user1 user2 =
      String.compare user1.name user2.name
  end
  module User_set = struct
    include Set.Make (User)
    let from_list li =
      List.fold_right add li empty
    let to_string users =
      String.concat ", "
        (List.map User.to_string
           (elements users))
  end

}}

let user_verify ~username ~password =
(*     if username = password then *)
    Some (User.create username)
(*     else None *)

(******************************************************************************)
(*                               Chat intestine                               *)

{shared{

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
    elt_id : Html5_types.div Html5.Id.id;
    prompt_id : Html5_types.input Html5.Id.id;
  }

  type chat_event =
    | Append of conversation * User.t
    | Remove of conversation

}}

type user_info = {
  user : User.t;
  chat_events : chat_event Eliom_react.Down.t;
  send_chat_event : chat_event -> unit;
}

let create_user_info user =
  let event, send = React.E.create () in
  { user;
    chat_events = Eliom_react.Down.of_react event;
    send_chat_event = fun x -> send x }

module Conversation_table =
  Weak.Make
    (struct
       type t = conversation
       let equal c1 c2 = c1.id = c2.id
       let hash { id } = id
     end)

let conversation_table = Conversation_table.create 13

let create_conversation =
  let next = counter () in
  fun users ->
    let conversation = {
      id = next ();
      bus = Eliom_bus.create ~scope:Eliom_common.site Json.t<conversation_event>;
      users;
      elt_id = Html5.Id.new_elt_id ();
      prompt_id = Html5.Id.new_elt_id ();
    } in
    Conversation_table.add conversation_table conversation;
    conversation

let fold_conversations f x =
  Conversation_table.fold f conversation_table x

let get_conversation id =
  let find conversation sofar =
    if conversation.id = id then
      Some conversation
    else sofar
  in
  fold_conversations find None

let get_conversations user =
  fold_conversations
    (fun conversation sofar ->
       if User_set.mem user conversation.users then
         conversation :: sofar
       else sofar)
    []
(******************************************************************************)
(*                                 User info                                  *)

let user_info_eref = Eliom_reference.eref ~scope:Eliom_common.session_group None

let get_other_user_info other =
  let state = Eliom_state.External_states.volatile_data_group_state (User.group_name other) in
  Eliom_reference.Ext.get state user_info_eref

let init_dialog =
  server_function
    (get_eref_option user_info_eref
       (fun _ -> Lwt.fail Not_allowed)
       (fun user_info other ->
          match_lwt get_other_user_info other with
            | Some other_user_info ->
                let users = User_set.from_list [user_info.user; other] in
                let conversation = create_conversation users in
                user_info.send_chat_event (Append (conversation, other));
                other_user_info.send_chat_event (Append (conversation, user_info.user));
                Lwt.return true
            | None ->
                Lwt.fail (Failure "init_dialog")))

let cancel_dialog =
  server_function
    (get_eref_option user_info_eref
       (fun _ -> Lwt.fail Not_allowed)
       (fun { user } conversation_id ->
          match get_conversation conversation_id with
            | Some conversation ->
                (match User_set.elements (User_set.remove user conversation.users) with
                   | [other] ->
                        (match_lwt get_other_user_info other with
                           | Some other_user_info ->
                               other_user_info.send_chat_event (Remove conversation);
                               Lwt.return ()
                           | None ->
                               Lwt.fail (Failure "cancel_dialog: Cannot get other user info"))
                   | _ -> Lwt.fail (Failure "cancel_dialog: Not a dialog"))
            | None ->
                Lwt.return ()))


(******************************************************************************)
(*                            Client processes                                *)

module Client_processes_user =
  Client_processes.Make
    (struct
       include User
       let get () =
         match_lwt Eliom_reference.get user_info_eref with
           | Some { user } -> Lwt.return user
           | None -> failwith "Client_processes_user.get"
     end)

let connected_users =
  React.S.map ~eq:User_set.equal
    (fun client_processes_user ->
       User_set.from_list
         (List.map snd
            (Int_map.bindings client_processes_user)))
    Client_processes_user.signal

let () =
  Lwt_react.E.keep
    (React.E.map
       (fun user ->
          debug "Removal of user %s" (User.to_string user);
          List.iter
            (fun conversation ->
               User_set.iter
                 (fun other ->
                    Lwt.ignore_result
                      (debug "HIC 0: %s" other.User.name;
                       match_lwt get_other_user_info other with
                         | Some other_user_info ->
                             debug "HIC 1";
                             other_user_info.send_chat_event (Remove conversation);
                             Lwt.return ()
                         | None ->
                             debug "HIC 2";
                             Lwt.return ()))
                 conversation.users)
            (get_conversations user))
       (removals User_set.diff User_set.elements connected_users))

let connected_users_down = Eliom_react.S.Down.of_react connected_users

(******************************************************************************)
(*                              User service                                  *)

let login_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

let logout_service =
  Eliom_service.post_coservice' ~post_params:Eliom_parameter.unit ()

let login_message_eref = Eliom_reference.Volatile.eref ~scope:Eliom_common.request None

let login_handler () (username, password) =
  match user_verify username password with
    | Some user ->
        Eliom_state.set_volatile_data_session_group ~scope:Eliom_common.session (User.group_name user);
        (match_lwt Eliom_reference.get user_info_eref with
           | Some _ ->
               Eliom_reference.Volatile.set login_message_eref (Some "Already logged in");
               Eliom_registration.Action.send ()
           | None ->
               let user_info = create_user_info user in
               lwt () = Eliom_reference.set user_info_eref (Some user_info) in
               Eliom_registration.Redirection.send Eliom_service.void_hidden_coservice')
    | None ->
        Eliom_reference.Volatile.set login_message_eref (Some "Wrong credentials");
        Eliom_registration.Action.send ()

let logout_handler () () =
  match_lwt Eliom_reference.get user_info_eref with
    | Some _ ->
        Client_processes_user.erase_process ();
        lwt () = Eliom_state.discard ~scope:Eliom_common.session () in
        Eliom_registration.Redirection.send Eliom_service.void_hidden_coservice'
    | None ->
        Eliom_registration.Action.send ()

(******************************************************************************)
(*                                  Widgets                                   *)

let info = Html5.D.(div [])
let connected_users_list = Html5.D.(ul ~a:[a_class ["users_list"]] [])
let conversations_id = Html5.Id.new_elt_id ()

{shared{

  let user_widget ?self user =
    let open Html5.F in
    let self_class =
      let self_class user' =
        if 0 = User.compare user user' then
          ["self"]
        else []
      in
      Option.get (fun () -> [])
        (Option.map self_class self)
    in
    span ~a:[a_class ("user_name" :: self_class);
             a_style ("background-color:"^user.User.color); ]
      [ pcdata user.User.name ]

  let message_widget user message =
    let open Html5.F in
    li ~a:[a_class ["message"]] [
      user_widget ~self:user message.author;
      span ~a:[a_class ["content"]]
        [pcdata message.content]
    ]
}}

{client{
  let cancel_dialog = %cancel_dialog
  let conversations_id = %conversations_id
}}

{shared{

  let conversation_widget user others conversation =
    let open Html5.F in
    let participants =
      let elts =
        List.map (fun other -> li [user_widget ~self:user other])
          (User_set.elements others)
      in
      Html5.D.ul ~a:[a_class ["participants"]] elts
    in
    let close =
      Html5.D.span ~a:[a_class ["close"]] [pcdata ""] (*WTF entity "#10060" - inserted in CSS*)
    in
    let messages = Html5.D.ul ~a:[a_class ["messages"]] [] in
    let prompt =
      Html5.Id.create_named_elt ~id:conversation.prompt_id
        (Html5.D.input ~a:[a_class ["prompt"]] ())
    in
    let conversation_elt =
      let participants_complete =
        div ~a:[a_class ["participants_complete"]] [
          span ~a:[a_class ["info_label"]] [ pcdata "With " ];
          participants;
          close;
        ]
      in
      Html5.Id.create_named_elt ~id:conversation.elt_id
        (Html5.D.div ~a:[a_class ["conversation"]]
           [ participants_complete; messages; (prompt :> Html5_types.div_content_fun Html5.elt); ])
    in
    let cancel_dialog = %cancel_dialog in
    let conversations_id = %conversations_id in
    ignore {unit{
      Eliom_client.withdom
        (fun () ->
           ignore
             (Html5.Manip.addEventListener %prompt Dom_html.Event.keypress
              (fun _ ev ->
                 if ev##keyCode = 13 then
                   (debug "Handle enter key press";
                    let prompt_dom = Html5.To_dom.of_input %prompt in
                    let content = Js.to_string prompt_dom##value in
                    prompt_dom##value <- Js.string "";
                    Lwt.ignore_result
                      (Eliom_bus.write %(conversation.bus)
                         (Message { author = %user; content }));
                    false)
                 else true));
           ignore
             (Html5.Manip.addEventListener %close Dom_html.Event.click
                (fun _ ev ->
                   let conversation_dom = Html5.To_dom.of_element %conversation_elt in
                   if Js.to_bool (conversation_dom##classList##contains(Js.string "disabled")) then
                      Html5.Manip.Named.removeChild %conversations_id %conversation_elt
                   else
                     Lwt.ignore_result
                       (try_lwt
                          lwt () = %cancel_dialog %(conversation.id) in
                          Html5.Manip.removeChild %conversation_elt %prompt;
                          conversation_dom##classList##add(Js.string "disabled");
                          Lwt.return ()
                        with exc ->
                          Eliom_lib.debug_exn "Cannot cancel dialog" exc;
                          Eliom_lib.error "Cannot cancel dialog");
                   true));
           let dispatch_message = function
             | Message msg ->
                 Html5.Manip.appendChild %messages (message_widget %user msg);
                 (let messages_dom = Html5.To_dom.of_element %messages in
                  messages_dom##scrollTop <- messages_dom##scrollHeight)
           in
           Lwt.ignore_result
              (Lwt_stream.iter dispatch_message
                 (Eliom_bus.stream %(conversation.bus))))
    }};
    conversation_elt
}}

{client{

  let focus_conversation conversation =
    (Html5.To_dom.of_input (Html5.Id.get_element conversation.prompt_id))##focus ()

  let user_list_user_widget user other =
    let onclick ev =
      Lwt.ignore_result ( %init_dialog other)
    in
    let open Html5.F in
    span ~a:[a_class ["user"]; a_onclick onclick]
      [ user_widget ~self:user other ]
}}

{client{

  let append_conversation conversation user other =
    Html5.Manip.Named.appendChild %conversations_id
      (conversation_widget user (User_set.from_list [other]) conversation);
    focus_conversation conversation

  let remove_conversation conversation user =
    let conversation_elt = Html5.Id.get_element conversation.elt_id in
    (Html5.To_dom.of_element conversation_elt)##classList##add(Js.string "disabled");
    Html5.Manip.removeChild conversation_elt
      (Html5.Id.get_element conversation.prompt_id)

}}

(******************************************************************************)
(*                              Main service                                  *)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let connected_main_handler { user; chat_events } =
  fun () () ->
    lwt id, fresh_process = Client_processes_user.assert_process () in
    if fresh_process then
      ignore {unit{
        Eliom_client.onload
          (fun () ->
             reflect_list_signal %connected_users_list
               (fun other -> [user_list_user_widget %user other])
               (React.S.map (fun users -> User_set.(elements (remove %user users)))
                  %connected_users_down);
             Lwt_react.E.keep
               (React.E.map
                  (function | Append (conversation, other) ->
                         append_conversation conversation %user other
                     | Remove conversation ->
                         remove_conversation conversation %user)
                  %chat_events);
             ())
      }};
    let conversations =
      Html5.D.div
        (List.map
           (fun conversation ->
              conversation_widget user
                (User_set.remove user conversation.users)
                conversation)
           (get_conversations user))
    in
    Lwt.return Html5.F.(
      html
        (Eliom_tools.Html5.head ~title:"Chat" ~css:[["chat.css"]] ())
        (body [
          p [a ~service:Eliom_service.void_coservice' [pcdata "Reload in app"] ()];
          div [
            b [pcdataf "Hello "];
            user_widget user;
            pcdataf " (at %d)" id;
            post_form ~xhr:false ~a:[a_class ["logout_form"]] ~service:logout_service
              (fun () -> [
                string_input ~input_type:`Submit ~value:"Logout" ();
              ]) ()
          ];
          div [
            b [pcdata "Users "];
            connected_users_list;
          ];
          Html5.Id.create_named_elt ~id:conversations_id conversations;
        ]))

let disconnected_main_handler () () =
  Lwt.return Html5.F.(
    html
      (Eliom_tools.Html5.head ~title:"Chat" ~css:[["chat.css"]] ())
      (body [
        h1 [pcdata "Chat"];
        div [
          post_form ~service:login_service
            (fun (username, password) -> [
              string_input ~input_type:`Text ~name:username ();
              br ();
              string_input ~input_type:`Text ~name:password ();
              br ();
              string_input ~input_type:`Submit ~value:"Login" ();
              br ();
            ] @
            (match Eliom_reference.Volatile.get login_message_eref with
               | Some msg -> [div [pcdata msg]]
               | None -> []))
            ()
        ]
      ]))

module Chat_app =
  Eliom_registration.App
    (struct let application_name = "chat" end)

let () =
  Chat_app.register ~service:main_service
    (get_eref_option_2 user_info_eref disconnected_main_handler connected_main_handler);
  Eliom_registration.Any.register ~service:login_service login_handler;
  Eliom_registration.Any.register ~service:logout_service logout_handler

