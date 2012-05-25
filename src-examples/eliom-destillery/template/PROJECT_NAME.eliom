
open Eliom_content

#ifndef MINIMAL_PROJECT
{shared{
  open Shared
}}
#endif
#ifdef MINIMAL_PROJECT
{client{
  let hello () =
    Dom_html.window##alert(Js.string "Phew, even some dynamic stuff!")
}}

module ##MODULE_NAME##_appl =
  Eliom_output.Eliom_appl (struct
    let application_name = "##PROJECT_NAME##"
  end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let counter =
  Eliom_reference.eref ~scope:Eliom_common.session 0

let main_handler () () =
  lwt count =
    lwt () = Eliom_reference.modify counter succ in
    Eliom_reference.get counter
  in
  let message = Printf.sprintf "Gratulations for the %d. time!" count in
  Lwt.return
    Html5.D.(html
      (Eliom_tools.Html5.head
         ~title:"##PROJECT_NAME##"
         ~css:[["css"; "##PROJECT_NAME##.css"; ]]
         ())
      (body [
       h1 [pcdata "Welcome to Eliom!"];
       p [
         pcdata message;
         a ~service:main_service [pcdata " ... and again ..."] ();
       ];
       p [span ~a:[a_onmouseover {{ hello () }}] [pcdata "hover me!"]];
      ]))
  let () =
    ##MODULE_NAME##_appl.register ~service:main_service main_handler
#else /* MINIMAL_PROJECT */

(********* Eliom references *********)
#ifdef BASIC_USER
let userid =
  Eliom_reference.eref
    ~persistent:"session_userid"
    ~scope:Eliom_common.session
    None

let wrongpassword =
  Eliom_reference.eref
    ~scope:Eliom_common.request
    false
#endif /* BASIC_USER */

let page content_elts =
#ifdef BASIC_USER
  lwt user_box =
    lwt user_opt = Eliom_reference.get userid >>= map_option_lwt Database.get_user in
    lwt wrongpassword = Eliom_reference.get wrongpassword in
    Lwt.return (Widgets.user_box user_opt wrongpassword)
  in
#endif /* BASIC_USER */
  Lwt.return Html5.D.(
    html
      (Eliom_tools.Html5.head
         ~title:"##PROJECT_NAME##"
         ~css:[["css"; "##PROJECT_NAME##.css";]]
         ())
      (body [
        h1 [pcdata "##PROJECT_NAME##"];
#ifdef BASIC_USER
        user_box;
#endif
        div content_elts;
      ])
  )

#ifdef BASIC_USER
(* The connection wrapper checks whether the user is connected,
   and if not displays the login page. If yes it behaves as the
   function given as parameters, taking user name, GET parameters and
   POST parameters.  *)
let connect_wrapper handler get_params post_params =
  lwt user_opt = Eliom_reference.get userid >>= map_option_lwt Database.get_user in
  match user_opt with
    | Some user ->
        handler user get_params post_params
    | None ->
        page [ Html5.D.pcdata "Not allowed. Log in first!" ]

let connect_service_handler () (email, pwd) =
  try_lwt
    lwt id = Database.check_pwd email pwd in
    lwt () = Eliom_reference.set userid (Some id) in
    Eliom_output.Redirection.send Eliom_service.void_hidden_coservice'
  with Not_found ->
    lwt () = Eliom_reference.set wrongpassword true in
    Eliom_output.Action.send ()

let signout_service_handler () () =
  Eliom_reference.unset userid

let important_service_handler user () () =
    page Html5.D.([
      h2 [pcdata "This is you"];
      table
        (tr [
          th [pcdata "Email"];
          td [pcdata user#!email];
        ]) [
        tr [
          th [pcdata "Password"];
          td [pcdata user#!pwd];
        ];
        tr [
          th [pcdata "First name"];
          td [pcdata user#!firstname];
        ];
        tr [
          th [pcdata "Last name"];
          td [pcdata user#!lastname];
        ];
        ]
    ])
#endif /* BASIC_USER */

let main_handler () () =
  page Html5.D.([
    p [pcdata "Can I has content, plz!"];
#ifdef BASIC_USER
    p [a ~service:Services.important_service [pcdata "los!"] ()];
#endif /* BASIC_USER */
  ])

let () =
#ifdef BASIC_USER
  Eliom_output.Any.register ~service:Services.connect_service
    connect_service_handler;
  Eliom_output.Action.register ~service:Services.signout_service
    signout_service_handler;
  Services.##MODULE_NAME##_appl.register ~service:Services.important_service
    (connect_wrapper important_service_handler);
#endif /* BASIC_USER */
  Services.##MODULE_NAME##_appl.register
    ~service:Services.main_service
    main_handler
#endif /* else MINIMAL_PROJECT */

