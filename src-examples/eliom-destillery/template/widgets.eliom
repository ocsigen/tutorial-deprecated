
open HTML5.M
open Eliom_output.Html5

#ifdef BASIC_USER
let class_userbox = "##PROJECT_NAME##_userbox"
let class_userbox_name = "##PROJECT_NAME##_userbox_name"

let login_form =
  post_form ~service:Services.connect_service
    (fun (loginname, pwdname) ->
      [fieldset [
        label ~a:[a_for loginname] [pcdata "Email: "];
        Eliom_output.Html5.string_input ~input_type:`Text ~name:loginname ();
        br ();
        label ~a:[a_for pwdname] [pcdata "Password: "];
        Eliom_output.Html5.string_input ~input_type:`Password ~name:pwdname ();
        br ();
        Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Connect" ()
      ]])
    ()

let signout_form user =
  post_form ~service:Services.signout_service
    (fun () -> [
      div [pcdata ("Hello "^user#!email)];
      br ();
      fieldset [
        string_input ~input_type:`Submit ~value:"Sign out" ()
      ]
    ]) ()

let user_box user_opt wrongpassword =
  div ~a:[a_class [class_userbox]]
    (match user_opt with
      | None ->
          if wrongpassword
          then [p [em [pcdata "Wrong login or password"]]; login_form]
          else [login_form]
      | Some user -> [
          p ~a:[a_class [class_userbox_name]] [pcdata user#!email];
          signout_form user])
#endif /* BASIC_USER */
