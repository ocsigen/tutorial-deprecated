
open Eliom_pervasives
open Utils

let debug fmt = debug_prefix "Chat_site" fmt

let login_message_ref = Eliom_references.eref ~scope:Eliom_common.request None

(** This is the implementation for both [Chat.USERS] and [User_management.USERS]
    to make their user systems compatible. *)
module Users = struct

  let user_count, set_user_count = Lwt_react.S.create 0

  module User = struct
    type t = {
      name : string;
      full_name : string;
      color : string;
    }
    let name { name } = name
    let full_name { full_name } = full_name
    let color { color } = color
    let id = name

    let make_session_group_name = Some id

    let compare = on name compare

    let create name full_name = 
      let color = Printf.sprintf "#%02x%02x%02x" (56+Random.int 200) (56+Random.int 200) (56+Random.int 200) in
      { name; full_name; color }

    let verify ~user_name ~password =
(*
      let l = String.length user_name in
      if l > 2 then
        let rec check i = if i = l then true else user_name.[l-i-1] = password.[i] && check (succ i) in
        if l = String.length password && check 0 then
          if Lwt_react.S.value user_count < 100 then
            Lwt.return **> Some (create user_name user_name)
          else (
            lwt () = Eliom_references.set login_message_ref (Some "Too many users") in
            Lwt.return None
          )
        else (
          Lwt.return None
        )
      else (
        lwt () = Eliom_references.set login_message_ref (Some "User name too short (at least 3 letters)") in
        Lwt.return None
      )
 *)
(*       try *)
(*         let password', user = List.assoc user_name !users_db in *)
(*         if password = password' then *)
          Lwt.return **> Some (create user_name user_name)
(*         else None *)
(*       with Not_found -> None *)
  end

  module User_set = Set.Make (User)

  let user_every_login_event, send_every_user_login_event = Lwt_react.E.create ()
  let user_final_logout_event, send_final_user_logout_event = Lwt_react.E.create ()

  module Callback = struct
    module User_map = Map.Make (User)
    let user_count = ref User_map.empty
    let get_count user =
      try User_map.find user !user_count with Not_found -> 0
    let modify_count f user =
      user_count :=
        User_map.filter
          (fun _ -> (<>) 0)
          (User_map.add user (f (get_count user)) !user_count);
      set_user_count (User_map.cardinal !user_count)
    let post_login user =
      debug "post_login %s" user.User.name;
      send_every_user_login_event user;
      modify_count succ user;
      Lwt.return ()
    let pre_logout ~session_group_size user =
      modify_count pred user;
      if get_count user = 0 then
        send_final_user_logout_event user;
      Lwt.return ()
  end
end

module My_scope = struct
  let scope_name = Eliom_common.create_scope_name "chat"
  let client_process = `Client_process scope_name
  let session = `Session scope_name
  let session_group = `Session_group scope_name 
end
(* or *)
(* module My_scope = Eliom_common *)

module Users' = struct
  module User = struct
    include Users.User
    (* Let's have the full name displayed for user management *)
    let name = Users.User.full_name
  end
  module User_set = Users.User_set
  module Callback = Users.Callback
end

module My_context = struct
  let disconnected content =
    lwt msg =
      Eliom_references.get login_message_ref >|= function
        | Some msg -> [HTML5.(p [pcdata msg])]
        | None -> []
    in
    Lwt.return HTML5.(
      h4 [pcdata "Welcome to Ocsigen Chat"]
      :: p [pcdata "To log in, please enter your username of choice and its reverse as the password ;-)"]
      :: content
      @ msg
    )
end

module My_user_management = User_management.Make (Users') (My_scope) (My_context)

module Connected_action = struct
  type user = Users.User.t
  include Eliom_output.Customize (Eliom_output.Action) (My_user_management.Connected_translate_action)
end
module My_chat = Chat.Make (Users.User) (Connected_action) (My_scope)


module Chat_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "chat_site"
    end
  )
module Connected_chat_appl = Eliom_output.Customize (Chat_appl) (My_user_management.Connected_translate_Html5)

let main_service = Eliom_services.service ~path:[] ~get_params:Eliom_parameters.unit ()

let main_handler =
  let mk_css_link path =
    let open Eliom_output.Html5_forms in
    let uri = make_uri (Eliom_services.static_dir ()) path in
    HTML5.create_global_elt HTML5.M.(css_link ~uri ()) in
  fun () () -> Lwt.return **>
    fun ~logout_form user ->
      lwt chat_body = My_chat.render user in
      let open HTML5.M in
      Lwt.return **> 
        html
          (head
             (title (pcdata "chat example"))
             (List.map mk_css_link (["example.css"] :: My_chat.css_files)))
          (body
             [(logout_form :> HTML5_types.body_content_fun HTML5.M.elt);
              chat_body])

(*
let chat_service =
  let aux ~path handler =
    Connected_chat_appl.register_service ~path ~get_params:Eliom_parameters.unit handler in
  My_chat.service aux ["chat"]
 *)

let () =
  Connected_chat_appl.register ~service:main_service main_handler;
  My_chat.set_client_process_timeout 1.0;
  My_chat.register ();
  ()

