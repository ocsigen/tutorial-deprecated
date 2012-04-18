
(********* Macaque initialisation *********)

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let get_db : unit -> unit Lwt_PGOCaml.t Lwt.t =
  let db_handler = ref None in
  fun () ->
    match !db_handler with
      | Some h -> Lwt.return h
      | None ->
          Lwt_PGOCaml.connect
            ~database:!Config.db_name
            ~user:!Config.db_user
            ()

#ifdef BASIC_USER
(********* Tables *********)

let users_table = <:table< users (
  userid bigint NOT NULL,
  email text NOT NULL,
  pwd text NOT NULL,
  firstname text NOT NULL,
  lastname text NOT NULL
) >>

(********* Queries *********)

let check_pwd email pwd =
  lwt dbh = get_db () in
  match_lwt
    Lwt_Query.query dbh
      <:select< row
      | row in $users_table$;
      row.email = $string:email$;
      row.pwd = $string:pwd$; >>
  with
    | [] -> Lwt.fail Not_found
    | [a] -> Lwt.return (a#!userid)
    | _ -> Lwt.fail (Failure "Several users have the same email")


let get_user id =
  lwt dbh = get_db () in
  match_lwt
    Lwt_Query.query dbh
      <:select< row |
        row in $users_table$;
        row.userid = $int64:id$ >>
  with
    | [] -> Lwt.fail Not_found
    | [user] -> Lwt.return user
    | _ -> Lwt.fail (Failure "Several users have the same userid")

let () =
  Lwt.ignore_result (
    lwt dbh = get_db () in
    match_lwt
      Lwt_Query.query dbh
        <:select< row |
          row in $users_table$; >>
    with
      | [] ->
          Lwt.return (print_endline "Create a user first!") (* TODO *)
      | _ -> Lwt.return ()
  )
#else /* BASIC_USER */
(********* Tables *********)

let articles_table = <:table< articles (
  id bigint NOT NULL,
  author text NOT NULL,
  content text NOT NULL
) >>

(********* Queries *********)

let get_articles () =
  lwt dbh = get_db () in
  Lwt_Query.query dbh
    <:select< row |
      row in $articles_table$ >>

let get_article id =
  lwt dbh = get_db () in
  match_lwt
    Lwt_Query.query dbh
      <:select< article |
        article in $articles_table$;
        article.id = $int64:id$ >>
  with
    | [] -> Lwt.fail Not_found
    | [article] -> Lwt.return article
    | _ -> Lwt.fail (Failure "Several articles with the same id")
#endif /* else BASIC_USER */
