
open Eliom_content
open Utils
open Shared

(* All confugations (of user verification, login- logout-forms, general UI) here *)

let user_verify ~user_name ~password =
  let l = String.length user_name in
  if l < 3 then
    lwt () = Eliom_reference.set User_management.login_error_ref (Some "User name too short (at least 3 letters)") in
    Lwt.return None
  else
    let rec check = function
      | i when i = l -> true
      | i -> user_name.[l-i-1] = password.[i] && check (succ i)
    in
    if l = String.length password && check 0 then
      Lwt.return **> Some (User.create user_name)
    (* Lwt.return **> Some (
        try
          List.find
            (function { User.name } -> name = user_name)
            (List.map fst **> Chat.get_all_user_infos ())
        with Not_found ->
          User.create user_name
      ) *)
    else
      lwt () = Eliom_reference.set User_management.login_error_ref (Some "Wrong password") in
      Lwt.return None

let login_form service =
  let open Html5.F in
  post_form ~a:[a_class ["login"]] ~service
    (fun (user_name, password) -> [
      table
        (tr [
          td [label ~a:[a_for user_name] [pcdata "Name"]];
          td [string_input ~a:[a_id "name"] ~input_type:`Text ~name:user_name ()]
         ])
        [tr [
          td [label ~a:[a_for password] [pcdata "Password"]];
          td [string_input ~a:[a_id "password"] ~input_type:`Password ~name:password ()]
         ];
         tr [
           td [];
           td [string_input ~input_type:`Submit ~value:"Login" ()]
         ]]
    ]) ()

let logout_form user service =
  let open Html5.F in
  post_form ~a:[a_class ["logout"]] ~service ~xhr:false
    (fun () -> [
      string_input ~input_type:`Submit ~value:"Logout" ();
    ]) ()

module Chat_app =
  Eliom_registration.App
    (struct let application_name = "chat_site" end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let connected user content =
  let open Html5.F in
  Html5.D.div ~a:[a_class ["connected"]] [
    (logout_form user User_management.logout_service :> Html5_types.div_content_fun elt);
    (content :> Html5_types.div_content_fun elt);
  ]

let disconnected ~message =
  let open Html5.F in
  let msg = span ~a:[a_class ["login_message"]] **>
    match message with
      | Some msg -> [pcdata msg]
      | None -> []
  in
  Html5.D.div ~a:[a_class ["disconnected"]] [
    h4 [pcdata "Welcome to Ocsigen Chat"];
    p [pcdata "To log in, please enter your username of choice \
               and its reverse as the password ;-)"];
    login_form User_management.login_service;
    msg;
  ]

let main_handler =
  fun () () ->
    lwt chat =
      User_management.connected_html5_elt connected disconnected
        Chat.render () ()
    in
    let tm = Unix.(gmtime (time ())) in
    Lwt.return Html5.F.(
      html
        (Eliom_tools.Html5.head
           ~title:"Eliom - Chat"
           ~css:(["example.css"] :: Chat.css_files) ())
        (body [
          div ~a:[a_class ["server_info"]] [
            pcdata **> Printf.sprintf "Sent from %s at %02d:%02d:%02d. "
              (Eliom_request_info.get_hostname ())
              tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;
            a ~service:Eliom_service.void_coservice' [pcdata "reload"] ();
          ];
          chat;
        ]))

let () =
  Chat.register ();
  User_management.register
    user_verify
    Chat.on_create_first_connection
    Chat.on_drop_last_connection;
  Chat_app.register ~service:main_service main_handler

