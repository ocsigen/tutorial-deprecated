
open Eliom_content
open Utils

let debug fmt = debug_prefix "User_management" fmt

module type USERS = sig

  module User : sig
    type t
    val name : t -> string
    val verify : user_name:string -> password:string -> t option Lwt.t
    val make_session_group_name : (t -> string) option
    val compare : t -> t -> int (* TODO Remove with get_volatile_data_session_group_size *)
  end


  module Callback : sig
    val post_login : User.t -> unit Lwt.t
    val pre_logout : session_group_size:int -> User.t -> unit Lwt.t
  end

end

module type SCOPE = sig
  val session : Eliom_common.session_scope
end

module No_callback = struct
  let post_login _ = Lwt.return ()
  let pre_logout _ _ = Lwt.return ()
end

module type CONTEXT = sig
  val disconnected : Html5_types.div_content_fun Html5.elt list -> Html5_types.body_content_fun Html5.elt list Lwt.t
end

module Identity_context = struct
  let disconnected elts = Lwt.return elts
end

module Make (Users : USERS) (Scope : SCOPE) (Context : CONTEXT) = struct

  open Users

  let modify_session_count, get_session_count =
    let module User_map = Map.Make (User) in
    let user_session_count_ref = ref User_map.empty in
    let get_session_count user =
      try User_map.find user !user_session_count_ref
      with Not_found -> 0
    in
    let modify_session_count f user =
      modify user_session_count_ref **>
        User_map.add user **> f (get_session_count user)
    in
    modify_session_count, get_session_count

  let login = Eliom_service.post_coservice' ~post_params:Eliom_parameter.(string "name" ** string "password") ()
  let logout = Eliom_service.post_coservice' ~post_params:Eliom_parameter.unit ()

  let user_ref = Eliom_reference.eref ~scope:Scope.session None
  let message_ref = Eliom_reference.eref ~scope:Eliom_common.request None

  let do_login user =
    iter_option (fun f -> Eliom_state.set_volatile_data_session_group ~scope:Scope.session (f user)) Users.User.make_session_group_name;
    lwt () = Eliom_reference.set user_ref (Some user) in
    lwt () = Callback.post_login user in
    Lwt.return **> modify_session_count succ user

  let do_logout user =
    (* TODO let session_group_size = Eliom_state.get_volatile_data_session_group_size ~scope:Scope.session in *)
    let session_group_size = get_session_count user in
    debug "Discard session (in session group of %d)" session_group_size;
    lwt () = Callback.pre_logout ~session_group_size user in
    lwt () = Eliom_state.discard_all_scopes () in
    Lwt.return **> modify_session_count pred user

  let _ =
    let login_handler () (user_name, password) =
      match_lwt Eliom_reference.get user_ref with
          None -> begin
            match_lwt User.verify ~user_name ~password with
                Some user -> do_login user
              | None -> Eliom_reference.set message_ref (Some "Invalid credentials")
          end
        | Some _ -> Lwt.return ()
    in
    let logout_handler () () =
      debug "logout_handler";
      match_lwt Eliom_reference.get user_ref with
          Some user -> do_logout user
        | None -> Lwt.return ()
    in
    Eliom_registration.Action.register ~service:login login_handler;
    Eliom_registration.Action.register ~service:logout logout_handler;
    ()

  module Connected_translate_Html5 = struct

    type page = logout_form:Html5_types.form Html5.elt -> User.t -> Eliom_registration.Html5.page Lwt.t

    let translate page =
      let login_form =
        let open Html5.D in
        post_form ~a:[a_class ["login"]] ~service:login
          (fun (user_name, password) -> [
            table
              (tr [
                td [label ~a:[Raw.a_for "name"] [pcdata "Name"]];
                td [string_input ~a:[a_id "name"] ~input_type:`Text ~name:user_name ()]
               ])
              [tr [
                td [label ~a:[Raw.a_for "password"] [pcdata "Password"]];
                td [string_input ~a:[a_id "password"] ~input_type:`Password ~name:password ()]
               ];
               tr [
                 td [];
                 td [string_input ~input_type:`Submit ~value:"Login" ()]
               ]]
          ]) ()
      in
      let logout_form user =
        let open Html5.D in
        post_form ~a:[a_class ["logout"]] ~service:logout ~xhr:false
          (fun () ->
             [pcdata "Logged in as ";
              span [pcdata **> User.name user];
              string_input ~input_type:`Submit ~value:"Logout" ()])
          ()
      in
      match_lwt Eliom_reference.get user_ref with
          None ->
            let open Html5.D in
            lwt message =
              match_lwt Eliom_reference.get message_ref with
                  None -> Lwt.return []
                | Some msg -> Lwt.return [p [pcdata msg]]
            in
            lwt body_content = Context.disconnected (login_form :: message) in
            Lwt.return **>
              html
                (head (title (pcdata "login")) [])
                (body body_content)
        | Some user ->
            page ~logout_form:(logout_form user :> Html5_types.form Html5.elt) user
  end

  module Connected_translate_action = struct

    type page = User.t -> unit Lwt.t

    let translate page =
      match_lwt Eliom_reference.get user_ref with
          None -> raise Eliom_common.Eliom_Session_expired
        | Some user -> page user
  end
end
