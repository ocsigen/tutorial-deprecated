
open Utils
open Shared

module Conversation_map = Map.Make (Conversation)
module User_map = Map.Make (User)
module User_set_map = Map.Make (User_set)

let id_for_conversation conversation =
  Printf.sprintf "conversation_%d" conversation.Conversation.id

let participants_class users =
  (* TODO We should use data attributes once they are in Tyxml *)
  "participants-" ^ (String.concat "-" **> List.map (string_of_int -| User.id) **> User_set.elements users)

let handle_enter_pressed msg_author prompt_dom bus ev =
  if ev##keyCode = 13 then begin
    let msg_content = Js.to_string prompt_dom##value in
    prompt_dom##value <- Js.string "";
    Lwt.ignore_result **> Eliom_bus.write bus { Conversation.msg_author; msg_content };
    Js._false
  end else Js._true

let dispatch_message user messages { Conversation.msg_author; msg_content } =
  let message =
    HTML5.(li [
      user_span ~self:user msg_author;
      span ~a:[a_class ["content"]] [pcdata msg_content]
    ])
  in
  Eliom_dom.appendChild messages message;
  (let messages_dom = Eliom_client.Html5.of_element messages in
   messages_dom##scrollTop <- messages_dom##scrollHeight)

(* Create a new conversation in the UI *)
let append_conversation user conversations conversation =
  debug "append_conversation between %s" (User_set.to_string conversation.Conversation.users);
  let conversation =
    let open HTML5 in
    let users_ul = ul ~a:[a_class ["participants"]] **>
      List.map (li -| singleton -| user_span ~self:user) **>
        User_set.elements **>
          User_set.filter (not -| User.equal user) **>
            conversation.Conversation.users
    in
    let messages = ul ~a:[a_class ["messages"]] [] in
    let prompt = input ~a:[a_class ["prompt"]] () in
    (* Setup the event handlers *)
    ignore **> Lwt_stream.iter
      (dispatch_message user messages)
      (Eliom_bus.stream conversation.Conversation.bus);
    (let prompt_dom = Eliom_client.Html5.of_input prompt in
     (* XXX Why is there no return value for the eventhandler in Eliom_dom? We then could use
        [Eliom_dom.addEventListener] *)
     ignore **> Dom_html.addEventListener
       prompt_dom
       Dom_html.Event.keypress
       (Dom_html.handler (handle_enter_pressed user prompt_dom conversation.Conversation.bus))
       Js._true);
    div ~a:[a_id **> id_for_conversation conversation;
            a_class ["conversation";
                     participants_class conversation.Conversation.users]]
      [div ~a:[a_class ["participants_complete"]]
         [span ~a:[a_class ["info_label"]]
            [pcdata "With: "];
          users_ul];
       messages; prompt]
  in
  Eliom_dom.appendChild conversations conversation

(* Remvoe a conversation from the UI and stop close its bus. *)
let remove_conversation conversation conversations =
  debug "remove_conversation between %s" (User_set.to_string conversation.Conversation.users);
  Eliom_bus.close conversation.Conversation.bus;
  Js.Opt.iter Dom_html.document##getElementById(Js.string **> id_for_conversation conversation)
    **> Dom.removeChild (Eliom_client.Html5.of_div conversations)

(* The main dispatching function for events on the client's [channel]' *)
let dispatch_event user conversations_elt = function
  | Append_conversation conversation ->
      append_conversation user conversations_elt conversation
  | Remove_conversation conversation ->
      remove_conversation conversation conversations_elt

let create_or_focus_conversation create_dialog_service user other =
  let users = List.fold_right User_set.add [user;other] User_set.empty in
  match Js.Opt.to_option Dom_html.document##querySelector(Js.string (".conversation."^participants_class users)) with
    | Some conversation ->
        debug "Focus existing conversation between %s" (User_set.to_string users);
        Js.Opt.iter
          (Dom_html.CoerceTo.input **>
             Js.Opt.get conversation##querySelector(Js.string "input")
               (fun _ -> failwith "conversation without input"))
          (fun input_elt -> input_elt##focus ())
    | None ->
        debug "No conversation_for_users found, create_dialog_service";
        Lwt.ignore_result **> Eliom_client.call_service create_dialog_service () other

(* Reflect changes of the [users] signal in the list of users in the UI *)
let change_users user users_elt create_dialog_service users =
  let create_user_li other =
    let user_span_elt = HTML5.li [user_span ~self:user other] in
    ignore **> Eliom_dom.addEventListener
      user_span_elt
      Dom_events.Typ.click
      (fun _ _ -> create_or_focus_conversation create_dialog_service user other);
    user_span_elt
  in
  Eliom_dom.replaceAllChild
    users_elt
    (List.map create_user_li **> User_set.elements users)

let onload_chat ~users_signal ~users_elt ~conversations_elt ~user ~channel ~conversations ~create_dialog_service =
  debug "onload chat";
  Eliom_comet.Configuration.(let c = new_configuration () in set_always_active c true);
  List.iter (append_conversation user conversations_elt) conversations;
  Lwt.ignore_result
    (try_lwt
       Lwt_stream.iter (dispatch_event user conversations_elt) channel
     with
       | Eliom_comet.Process_closed ->
           Eliom_client.exit_to ~service:Eliom_services.void_coservice' () ();
           Lwt.return ()
       | e ->
           debug_exn "Exception while streaming" e;
           Lwt.return ());
  let other_users_signal = Lwt_react.S.map (User_set.filter (not -| User.equal user)) users_signal in
  Lwt_react.S.(keep **> map (change_users user users_elt create_dialog_service) other_users_signal)
