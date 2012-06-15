
{shared{
  open Eliom_content
  open Eliom_content.Html5.F
  open Html5_types
  open Utils
  open Shared
}}

{shared{

  let user_span ?self user =
    let self_class =
      let self_class user' =
        if User.equal user user' then
          ["self"]
        else []
      in
      Option.get (fun () -> []) **>
        Option.map self_class self
    in
    let a = [
      a_class ("user_name" :: self_class);
      a_style ("background-color: "^user.User.color)
    ] in
    span ~a [ pcdata user.User.name ]

}}

{client{

  let create_user_li ?self ~on_click user =
    let user_span_elt = Html5.D.li [user_span ?self user] in
    ignore **> Html5.Manip.addEventListener
      user_span_elt
      Dom_html.Event.click
      (fun _ _ ->
         debug "User %s clicked" user.User.name;
         on_click ();
         true);
    user_span_elt

  let users_lis ~self users =
    List.map (li -| singleton -| user_span ~self) **>
      User_set.elements **> User_set.filter (not -| User.equal self) **>
        users

  let conversation id handle_prompt_keypress on_close user users =
    let users_ul =
      Html5.D.ul ~a:[a_class ["participants"]] **>
        users_lis ~self:user users
    in
    let messages = Html5.D.ul ~a:[a_class ["messages"]] [] in
    let prompt = Html5.D.input ~a:[a_class ["prompt"]] () in
    let close =
      Html5.D.span ~a:[a_class ["close"]] [pcdata "×"] (*WTF entity "#10060"*)
    in
    Eliom_lib.jsdebug close;
    let a = [
      a_id **> id;
      a_class ["conversation"];
    ] in
    let conversation_elt =
      Html5.D.div ~a [
        div ~a:[a_class ["participants_complete"]] [
          span ~a:[a_class ["info_label"]] [ pcdata "With: " ];
          users_ul;
          close;
        ];
        messages;
        prompt;
      ]
    in
    ignore **> Html5.Manip.addEventListener prompt Dom_html.Event.keypress
      (fun _ -> handle_prompt_keypress prompt);
    ignore **> Html5.Manip.addEventListener close Dom_html.Event.click
      (fun _ -> on_close conversation_elt);
    conversation_elt, messages, prompt

  let message user message =
    li [
      user_span ~self:user message.author;
      span ~a:[a_class ["content"]]
        [pcdata message.content]
    ]
}}

{server{

  let main_id = Html5.Id.new_elt_id ~global:false ()
  let users_id = Html5.Id.new_elt_id ()
  let conversations_id = Html5.Id.new_elt_id ()

  let main user onload =
    Html5.Id.create_named_elt ~id:main_id **>
      Html5.D.div ~a:[a_class ["chat"]; a_onload onload] [
        div ~a:[a_class ["user_and_users"]] [
          h3 [
            pcdata "Hello ";
            user_span ~self:user user;
          ];
          span ~a:[a_class ["info_label"]] [
            pcdata "Users: ";
          ];
          Html5.Id.create_named_elt ~id:users_id
            (div ~a:[a_class ["users_list"]] []);
          span ~a:[a_class["note"]] [
            pcdata " (Click one to start a conversation)"
          ]
        ];
        Html5.Id.create_named_elt ~id:conversations_id
          (div ~a:[a_class ["conversations_list"]] []);
      ]
}}
