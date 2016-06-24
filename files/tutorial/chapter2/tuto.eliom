open Eliom_content.Html.D
open Eliom_parameter

(* Services *)

let main_service = Eliom_service.create
  ~id:(Eliom_service.Path [""])
  ~meth:(Eliom_service.Get unit)
  ()

let user_service  = Eliom_service.create
  ~id:(Eliom_service.Path ["users"])
  ~meth:(Eliom_service.Get (string "name" |> suffix))
  ()

let connection_service = Eliom_service.create
  ~id:(Eliom_service.Global)
  ~meth:(Eliom_service.Post (unit, string "name" ** string "password"))
  ()

let disconnection_service = Eliom_service.create
  ~id:(Eliom_service.Global)
  ~meth:(Eliom_service.Post (unit,unit))
  ()

let new_user_form_service = Eliom_service.create
  ~id:(Eliom_service.Path ["create account"])
  ~meth:(Eliom_service.Get unit)
  ()

let account_confirmation_service =
  Eliom_service.create
    ~id:(Eliom_service.Fallback new_user_form_service)
    ~meth:(Eliom_service.Post (unit, string "name" ** string "password"))
    ()

(* User names and passwords: *)
let users = ref [("Calvin", "123"); ("Hobbes", "456")]

let user_links =
  let link_of_user = fun (name, _) ->
    li [a ~service:user_service [pcdata name] name]
  in
  fun () -> ul (List.map link_of_user !users)

let check_pwd name pwd =
  try List.assoc name !users = pwd with Not_found -> false



(* Eliom references *)
let username =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    None

let wrong_pwd =
  Eliom_reference.eref
    ~scope:Eliom_common.request_scope
    false

(* Page widgets: *)
let disconnect_box () =
  Form.post_form disconnection_service
    (fun _ ->
      [fieldset [Form.input ~input_type:`Submit ~value:"Log out" Form.string]]
    )
    ()

let connection_box () =
  let%lwt u = Eliom_reference.get username in
  let%lwt wp = Eliom_reference.get wrong_pwd in
  Lwt.return
    (match u with
      | Some s -> div [p [pcdata "You are connected as "; pcdata s; ];
                       disconnect_box () ]
      | None ->
        let l =
          [Form.post_form ~service:connection_service
            (fun (name1, name2) ->
              [fieldset
		  [label [pcdata "login: "];
                   Form.input ~input_type:`Text ~name:name1 Form.string;
                   br ();
                   label [pcdata "password: "];
                   Form.input ~input_type:`Password ~name:name2 Form.string;
                   br ();
                   Form.input ~input_type:`Submit ~value:"Connect" Form.string
                 ]]) ();
             p [a new_user_form_service [pcdata "Create an account"] ()]]
        in
        if wp
        then div ((p [em [pcdata "Wrong user or password"]])::l)
        else div l
    )

let create_account_form () =
  Form.post_form ~service:account_confirmation_service
    (fun (name1, name2) ->
      [fieldset
	 [label [pcdata "login: "];
          Form.input ~input_type:`Text ~name:name1 Form.string;
          br ();
          label [pcdata "password: "];
          Form.input ~input_type:`Password ~name:name2 Form.string;
          br ();
          Form.input ~input_type:`Submit ~value:"Connect" Form.string
         ]]) ()

(* Registration of services *)
let _ =
  Eliom_registration.Html.register
    ~service:main_service
    (fun () () ->
      let%lwt cf = connection_box () in
      html
	(head (title (pcdata "")) [])
        (body [h1 [pcdata "Hello"];
               cf;
               user_links ()
	      ])
	|> Lwt.return);

  Eliom_registration.Any.register
    ~service:user_service
    (fun name () ->
      if List.exists (fun (n, _) -> n = name) !users
      then begin
        let%lwt cf = connection_box () in
        Eliom_registration.Html.send
          (html (head (title (pcdata name)) [])
             (body [h1 [pcdata name];
                    cf;
                    p [a ~service:main_service [pcdata "Home"] ()]]))
      end
      else
        Eliom_registration.Html.send
          ~code:404
          (html (head (title (pcdata "404")) [])
             (body [h1 [pcdata "404"];
                    p [pcdata "That page does not exist"]]))
    );

  Eliom_registration.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      if check_pwd name password
      then Eliom_reference.set username (Some name)
      else Eliom_reference.set wrong_pwd true);

  Eliom_registration.Action.register
    ~service:disconnection_service
    (fun () () -> Eliom_state.discard ~scope:Eliom_common.default_session_scope ());

  Eliom_registration.Html.register
    ~service:new_user_form_service
    (fun () () ->
      Lwt.return
        (html (head (title (pcdata "")) [])
              (body [h1 [pcdata "Create an account"];
                     create_account_form ();
                    ])));

  Eliom_registration.Html.register
    ~service:account_confirmation_service
    (fun () (name, pwd) ->
      let create_account_service =
	Eliom_service.create
          ~id:(Eliom_service.Fallback main_service)
          ~meth:(Eliom_service.Get unit)
          ~timeout:60.
	  ~max_use:1
	  ()
      in
      let _ =
        Eliom_registration.Action.register
	  ~service:create_account_service
          (fun () () ->
            users := (name, pwd)::!users;
            Lwt.return ())
      in
      Lwt.return
        (html (head (title (pcdata "")) [])
              (body [h1 [pcdata "Confirm account creation for "; pcdata name];
                     p [a ~service:create_account_service [pcdata "Yes"] ();
                        pcdata " ";
                        a ~service:main_service [pcdata "No"] ()]
                    ])))
