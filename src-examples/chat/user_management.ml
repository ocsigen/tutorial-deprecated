
open Utils
open Shared

(* Services *)

let login_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

let logout_service =
  Eliom_service.post_coservice' ~post_params:Eliom_parameter.unit ()

(* References *)

let user_ref = Eliom_reference.eref ~scope:Eliom_common.session None
let login_error_ref = Eliom_reference.eref ~scope:Eliom_common.request None

(* Connected handler auxiliaries *)

let connected handler disconnected =
  fun get post ->
    match_lwt Eliom_reference.get user_ref with
      | Some user ->
          handler user get post
      | None ->
          disconnected get post

let connected_html5_elt connected_widget disconnected_widget handler =
  connected
    (fun user get post ->
       lwt content = handler user get post in
       Lwt.return **>
         connected_widget user content)
    (fun get post ->
       lwt message = (Eliom_reference.get login_error_ref : string option Lwt.t) in
       Lwt.return **>
         disconnected_widget ~message)

let connected_action handler =
  connected handler
    (fun get post ->
       Lwt.fail Eliom_common.Eliom_Session_expired)

(* Connections *)

let on_create_first_connection = ref **> fun _ -> failwith "User_management.on_create_first_connection"
let on_drop_last_connection = ref **> fun _ -> failwith "User_management.on_drop_last_connection"

let on_create_connection, on_loose_connection, users_signal =
  let users_signal, set_users_signal = React.S.create User_set.empty in
  let module Connection_count = Map.Make (User) in
  let connection_count = ref Connection_count.empty in
  let get user = try Connection_count.find user !connection_count with Not_found -> 0 in
  let modify f user =
    connection_count :=
      Connection_count.filter (fun _ -> (<>) 0) **>
        Connection_count.add user (f (get user)) !connection_count;
    debug "connection_count: %s" **>
      String.concat ", " **>
        List.map (fun (u, c) -> Printf.sprintf "%s: %d" u.User.name c) **>
          Connection_count.bindings !connection_count;
  in
  let add user =
    modify succ user;
    if get user = 1 then
      let () = !on_create_first_connection user in
      set_users_signal (User_set.add user (React.S.value users_signal))
  in
  let remove user =
    modify pred user;
    if get user = 0 then
      let () = !on_drop_last_connection user in
      set_users_signal (User_set.remove user (React.S.value users_signal))
  in
  add, remove, users_signal

let init_connection user =
  on_create_connection user;
  Lwt.ignore_result (
    lwt () = Eliom_comet.Channel.wait_timeout 1.0 in
    debug "wait_timeout for user %s" user.User.name;
    on_loose_connection user;
    Lwt.return ()
  )

(* Handlers *)

let login_handler user_verify () (user_name, password) =
  match_lwt user_verify ~user_name ~password with
    | Some user ->
        Eliom_state.set_volatile_data_session_group ~scope:Eliom_common.session user.User.name;
        lwt () = Eliom_reference.set user_ref (Some user) in
        Eliom_registration.Redirection.send Eliom_service.void_hidden_coservice'
    | None ->
        Eliom_registration.Action.send ()

let logout_handler () () =
  debug "logout_handler";
  match_lwt Eliom_reference.get user_ref with
    | Some user ->
        lwt () = Eliom_state.discard ~scope:Eliom_common.session () in
        lwt () = Eliom_state.discard ~scope:Eliom_common.client_process () in
        Eliom_registration.Redirection.send Eliom_service.void_hidden_coservice'
    | None ->
        Eliom_registration.Action.send ()

(* Registration *)

let register user_verify on_create_first_connection' on_drop_last_connection' =
  on_create_first_connection := on_create_first_connection';
  on_drop_last_connection := on_drop_last_connection';
  let on_users_change = debug "Active user connection: %s" -| User_set.to_string in
  Lwt_react.S.(keep **> map on_users_change users_signal);
  Eliom_registration.Any.register ~service:login_service (login_handler user_verify);
  Eliom_registration.Any.register ~service:logout_service logout_handler
