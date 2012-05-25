
open Eliom_content
open Utils
open Shared

let debug fmt = debug_prefix "Chat" fmt

module type USER = sig
  type t
  val compare : t -> t -> int
  val id : t -> int
  val name : t -> string
  val color : t -> string
end

module type SCOPE = sig
  val session_group : Eliom_common.session_group_scope
  val session : Eliom_common.session_scope
  val client_process : Eliom_common.client_process_scope
end

module type ACTION = sig
  type user
  val register :
    ?scope:[< Eliom_common.scope ] -> ?options:Eliom_output.Action.options -> ?charset:string ->
    ?code:int -> ?content_type:string -> ?headers:Http_headers.t -> ?secure_session:bool ->
    service:('a, 'b, [< Eliom_service.internal_service_kind ], [< Eliom_service.suff ], 'c, 'd, [ `Registrable ], Eliom_output.Action.return) Eliom_service.service ->
    ?error_handler:((string * exn) list -> (user -> unit Lwt.t) Lwt.t) ->
    ('a -> 'b -> (user -> unit Lwt.t) Lwt.t) -> unit
end

(* Work-around as its not possible to use {{ ... }}  syntax within the following functor *)
let onload_chat_event_handler ~users_signal ~users_elt ~conversations_elt ~user ~channel ~conversations ~create_dialog_service = {{
  debug "onload_chat_event_handler";
  Client.onload_chat
    ~users_signal:%users_signal
    ~users_elt:%users_elt
    ~conversations_elt:%conversations_elt
    ~user:%user
    ~channel:%channel
    ~conversations:%conversations
    ~create_dialog_service:%create_dialog_service
}}

module Make (ForeignUser : USER) (Action : ACTION with type user = ForeignUser.t) (Scope : SCOPE) = struct

  (* A service for triggering the creation of a dialog (i.e. a conversation of
     2 participants). *)
  let create_dialog_service =
    Eliom_service.post_coservice' ~post_params:(User.parameter "user") ()

  let css_files = [["chat.css"]]

  let import_user : ForeignUser.t -> User.t =
    fun user -> {
      User.id = ForeignUser.id user;
      name = ForeignUser.name user;
      color = ForeignUser.color user;
    }

  let users_signal, modify_users_signal = 
    let users_signal, set = Lwt_react.S.create ~eq:User_set.equal User_set.empty in
    let modify_users_signal f =
      let users = f **> Lwt_react.S.value users_signal in
      debug "modify_users_signal to %s" (User_set.to_string users);
      set users
    in
    Eliom_react.S.Down.of_react ~name:"users_signal" ~scope:Eliom_common.site users_signal,
    modify_users_signal

  module User_info = struct
    type key = User.t
    type t = {
      id : int;
      stream : event Lwt_stream.t;
      send : event -> unit;
      mutable conversations : Conversation.t list;
    }
    let prototype = {
      id = 0;
      stream = Lwt_stream.from undefined;
      send = undefined;
      conversations = []
    }
    let create =
      let counter = ref 0 in
      fun user ->
        let stream, send = Lwt_stream.create () in
        { id = (incr counter; !counter); stream = stream; send = send -| some; conversations = [] }
  end

  (* The following 4 functions deal with providing each user's [User_info.t].
     In the first place, each user's [User_info.t] is stored in a corresponding
     Eliom reference of scope session group. As we want to access every user's
     [User_info.t] for sending him messages, it is additionally stored in a
     weak array which can be arbitrarily accessed. *)
  let set_user, get_current_user_info, get_user_info, get_all_users_infos =
    let module User_info_table = Weak_info.Make (User) (User_info) in
    let user_info_ref = Eliom_reference.eref ~scope:Scope.session_group None in
    let user_info_table = User_info_table.create 13 in
    let set_user user =
      debug "Set user %s" (User.name user);
      Eliom_state.set_volatile_data_session_group ~scope:Scope.session (string_of_int **> User.id user);
      match_lwt Eliom_reference.get user_info_ref with
          None ->
            let user_info = User_info.create user in
            let data = User_info_table.add_by_key user_info_table user user_info in
            debug "No data yet, create it (%d) and store it in a eliom reference of scope session group, and in a global, weak array."
              user_info.User_info.id;
            Eliom_reference.set user_info_ref (Some data)
        | Some _ -> Lwt.return ()
    in
    let get_current_user_info user =
      match_lwt Eliom_reference.get user_info_ref with
          Some user_info -> Lwt.return (User_info_table.E.info user_info)
        | None ->
            lwt () = set_user user in
            Lwt.map (User_info_table.E.info -| get_some) **> Eliom_reference.get user_info_ref
    in
    let get_user_info user =
      User_info_table.find_by_key user_info_table user
    in
    let get_all_users_infos () =
      User_info_table.fold (cons -| User_info_table.E.key_info) user_info_table []
    in
    set_user, get_current_user_info, get_user_info, get_all_users_infos

  (* [add_mutual_conversations users] triggers the creation of a conversation
     between the users given in [users]. *)
  let add_mutual_conversations users =
    debug "Add mutual conversation between %s" (User_set.to_string users);
    let conversation =
      let bus = Eliom_bus.create Json.t<Conversation.message> in
      Conversation.create bus users
    in
    let for_user_info user =
      let user_info = get_user_info user in 
      user_info.User_info.conversations <- conversation :: user_info.User_info.conversations;
      user_info.User_info.send **> Append_conversation conversation
    in
    User_set.iter for_user_info users

  let tear_down_conversations user =
    debug "Tearing down conversations for user %s" (User.name user);
    let user_info = get_user_info user in
    flip List.iter user_info.User_info.conversations **> begin fun conversation ->
      flip User_set.iter conversation.Conversation.users **> fun user' ->
        let user_info' = get_user_info user' in
        user_info'.User_info.send **> Remove_conversation conversation;
        user_info'.User_info.conversations <-
          List.filter ((<>) 0 -| Conversation.compare conversation)
            user_info'.User_info.conversations;
    end;
    user_info.User_info.conversations <- []

  let add_client_process, on_timeout_client_process =
    let client_process_counts = Hashtbl.create 13 in
    (fun user ->
      let count = try Hashtbl.find client_process_counts user with Not_found -> 0 in
      debug "Add client process count for user %s from %d" (User.name user) count;
      if count = 0 then
        modify_users_signal (User_set.add user);
      Hashtbl.add client_process_counts user (succ count)),
    (fun user ->
      let count = Hashtbl.find client_process_counts user in
      debug "Remove client process count for user %s from %d" (User.name user) count;
      if count = 1 then (
        modify_users_signal (User_set.remove user);
        tear_down_conversations user
      );
      Hashtbl.add client_process_counts user (pred count))

  let render_user = import_user |- fun user ->
    user_span ~self:user user

  let render_users ~id () =
    Html5.Id.create_named_elt ~id
      Html5.D.(ul ~a:[a_class ["users_list"]] [])

  let render_conversations ~id () =
    Html5.Id.create_named_elt ~id
      Html5.D.(div ~a:[a_class ["conversations"]] [])

  let client_process_timout = ref 1.0
  let set_client_process_timeout = set client_process_timout

  let render_onload :
    ForeignUser.t ->
    users_elt:Html5_types.ul Html5.elt ->
    conversations_elt:Html5_types.div Html5.elt ->
    #Dom_html.event Xml.caml_event_handler Lwt.t
  =
    let scope = Scope.client_process in
    let channel_ref = Eliom_reference.eref ~scope None in
    fun user ~users_elt ~conversations_elt ->
      debug "render_onload";
      let user = import_user user in
      lwt user_info = get_current_user_info user in
      lwt channel =
        match_lwt Eliom_reference.get channel_ref with
            Some channel ->
              Lwt.return channel
          | None ->
              debug "No client process channel for user %s, creating one and junk old messages" (User.name user);
              let stream = Lwt_stream.clone user_info.User_info.stream in
              let stream = flip Lwt_stream.map stream **> tap **> fun ev ->
                debug "Event %s for client process of user %s with info %d"
                  (event_to_string ev) (User.name user) user_info.User_info.id
              in
              lwt () = Lwt_stream.junk_old stream in
              let channel = Eliom_comet.Channels.create ~name:"channel" ~scope stream in
              lwt () = Eliom_reference.set channel_ref (Some channel) in
              let () = add_client_process user in
              Lwt.ignore_result (
                lwt () = Eliom_comet.Channels.wait_timeout ~scope !client_process_timout in
                lwt () = Eliom_reference.set channel_ref None in
                lwt () = Eliom_state.discard ~scope () in
                Lwt.return (on_timeout_client_process user)
              );
              Lwt.return channel
      in
      let conversations = user_info.User_info.conversations in
      Lwt.return **> onload_chat_event_handler
        ~users_signal ~users_elt ~conversations_elt ~user
        ~channel ~conversations ~create_dialog_service

  let users_id = Eliom_reference.eref_from_fun ~scope:Eliom_common.session Html5.Id.new_elt_id
  let conversations_id = Eliom_reference.eref_from_fun ~scope:Eliom_common.session Html5.Id.new_elt_id

  let render user =
    lwt users_id = Eliom_reference.get users_id in
    lwt conversations_id = Eliom_reference.get conversations_id in
    let user_elt = render_user user in
    let users_elt = render_users ~id:users_id () in
    let conversations_elt = render_conversations ~id:conversations_id () in
    lwt onload = render_onload user ~users_elt ~conversations_elt in
    let open Html5.D in
    Lwt.return **>
      div ~a:[a_class ["chat"]; a_onload onload] [
        div ~a:[a_class ["user_and_users"]] [
          h3 [
            pcdata "Hello ";
            user_elt
          ];
          span ~a:[a_class["info_label"]] [
            pcdata "Users: "
          ];
          (users_elt :> Html5_types.div_content_fun Html5.elt);
          span ~a:[a_class["note"]] [
            pcdata " (Click one to start a conversation)"
          ]
        ];
        conversations_elt
      ]

  let create_dialog_handler =
    fun () other ->
      Lwt.return (import_user |- fun user ->
          let users = User_set.(add user (add other empty)) in
          Lwt.return **> add_mutual_conversations users)

  let register () =
    Action.register ~service:create_dialog_service create_dialog_handler

end

