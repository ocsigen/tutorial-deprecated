
open Eliom_content
open Utils
open Shared

let onload user events conversations
      users_signal users_id conversations_id create_dialog_service cancel_dialog_service =

  debug "onload_chat";

  let focus_or_create_dialog other =
    let users = User_set.(elements **> add user (add other empty)) in
    try
      let conversation =
        List.find
          (fun c ->
             users = Html5.Custom_data.get_dom c participants_data &&
               not (Js.to_bool **> c##classList##contains(Js.string "disabled")))
          (Dom.list_of_nodeList **>
             Dom_html.document##querySelectorAll(Js.string ".conversation"))
      in
      Js.Opt.iter
        (Dom_html.CoerceTo.input **>
           Js.Opt.get conversation##querySelector(Js.string "input")
             (fun _ -> failwith "conversation without input"))
        (fun input_elt -> input_elt##focus ())
    with Not_found ->
      Lwt.ignore_result **>
        Eliom_client.call_service create_dialog_service () other
  in

  let change_users users =
    debug "change_users: %s" (User_set.to_string users);
    let create_user_li other =
      let on_click () =
        debug "onclick_user %s" other.User.name;
        focus_or_create_dialog other
      in
      Widgets.create_user_li ~on_click other
    in
    Html5.Manip.Named.replaceAllChild
      users_id
      (List.map create_user_li **> User_set.elements users)
  in

  let append_conversation conversation users =
    let handle_prompt_keypress prompt ev =
      if ev##keyCode = 13 then (
        debug "Handle return";
        let prompt_dom = Html5.To_dom.of_input prompt in
        let content = Js.to_string prompt_dom##value in
        prompt_dom##value <- Js.string "";
        Lwt.ignore_result **>
          Eliom_bus.write conversation.bus
            (Message { author = user; content });
        false
      ) else true
    in
    let on_close conversation_elt _ =
      debug "On close conversation";
      Lwt.ignore_result **>
        Eliom_client.call_service cancel_dialog_service () conversation.Shared.id;
      false
    in
    let id = id_of_conversation conversation in
    let conversation_elt, messages_elt, prompt =
      Widgets.conversation id handle_prompt_keypress on_close user users
    in
    let dispatch_message = function
      | Message message ->
          Html5.Manip.appendChild messages_elt (Widgets.message user message);
          (let messages_dom = Html5.To_dom.of_element messages_elt in
           messages_dom##scrollTop <- messages_dom##scrollHeight)
    in
    Html5.Custom_data.set_dom
      (Html5.To_dom.of_element conversation_elt)
      participants_data
      (User_set.elements users);
    Lwt.ignore_result **>
      Lwt_stream.iter dispatch_message
        (Eliom_bus.stream conversation.bus);
    Html5.Manip.Named.appendChild
      conversations_id
      conversation_elt;
    (Html5.To_dom.of_input prompt)##focus ();
  in

  let dispatch_event = function
    | Append_conversation (conversation, users) ->
        debug "Append conversation between %s" (User_set.to_string users);
        append_conversation conversation users
    | Remove_conversation conversation ->
        debug "Remove conversation %d" conversation.id;
        let disable_conversation conversation_dom =
          conversation_dom##classList##add(Js.string "disabled");
          Js.Opt.iter
            conversation_dom##querySelector(Js.string "input")
            (fun input_dom ->
               ignore conversation_dom##removeChild((input_dom :> Dom.node Js.t)));
          Js.Opt.iter
            conversation_dom##querySelector(Js.string ".close")
            (fun close_dom ->
               close_dom##onclick <-
                 Dom.handler **> fun _ ->
                   Html5.Manip.Named.removeChild conversations_id
                     (Html5.Of_dom.of_element conversation_dom);
                   Js._false)
        in
        Js.Opt.iter
          (Dom_html.document##querySelector(Js.string ("#"^id_of_conversation conversation)))
          disable_conversation;
        Eliom_bus.close conversation.bus
  in

  List.iter (uncurry append_conversation) conversations;
  Eliom_comet.Configuration.(let c = new_configuration () in set_always_active c true);
  Lwt_react.E.(keep **> map dispatch_event events);
  Lwt_react.S.(keep **>
    map change_users **>
      map (User_set.filter (not -| User.equal user)) users_signal);
  ()

