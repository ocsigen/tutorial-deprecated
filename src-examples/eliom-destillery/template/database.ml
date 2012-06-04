
(********* Macaque initialisation *********)

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_pgocaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_query = Query.Make_with_Db(Lwt_thread)(Lwt_pgocaml)

(****************************** Connection pool ******************************)

let connection_pool : (Lwt_pgocaml.pa_pg_data Lwt_pgocaml.t) Lwt_pool.t =
  let connect () =
    Lwt_pgocaml.connect
      ~database:!Config.db_name
      ~user:!Config.db_user
      ()
  in
  let validate dbh =
    try
      lwt () = Lwt_pgocaml.ping dbh in
      Lwt.return true
    with _ ->
      Lwt.return false
  in
  Lwt_pool.create 13 ~validate connect

#ifdef BASIC_USER
(*********************************** Tables **********************************)

let users_table = <:table< users (
  userid bigint NOT NULL,
  email text NOT NULL,
  pwd text NOT NULL,
  firstname text NOT NULL,
  lastname text NOT NULL
) >>

(********************************** Queries **********************************)

let check_pwd email pwd =
  Lwt_pool.use connection_pool
    (fun dbh ->
       match_lwt
         Lwt_query.query dbh
           <:select< u |
               u in $users_table$;
               u.email = $string:email$;
               u.pwd = $string:pwd$; >>
       with
         | [] -> Lwt.fail Not_found
         | [a] -> Lwt.return (a#!userid)
         | _ -> Lwt.fail (Failure "Several users have the same email"))


let get_user id =
  Lwt_pool.use connection_pool
    (fun dbh ->
       match_lwt
         Lwt_query.query dbh
           <:select< u |
               u in $users_table$;
               u.userid = $int64:id$ >>
       with
         | [] -> Lwt.fail Not_found
         | [user] -> Lwt.return user
         | _ -> Lwt.fail (Failure "Several users have the same userid"))

let () =
  Lwt.ignore_result
    (Lwt_pool.use connection_pool
       (fun dbh ->
          match_lwt
            Lwt_query.query dbh
              <:select< u |
                u in $users_table$; >>
          with
            | [] ->
                Lwt.return (print_endline "Create a user first!") (* TODO *)
            | _ -> Lwt.return ()))
#else /* BASIC_USER */
(*********************************** Tables **********************************)

let articles_table = <:table< articles (
  id bigint NOT NULL,
  author text NOT NULL,
  content text NOT NULL
) >>

(********************************** Queries **********************************)

let get_articles () =
  Lwt_pool.use connection_pool
    (fun dbh ->
       Lwt_query.query dbh
         <:select< article |
           article in $articles_table$ >>)

let get_article id =
  Lwt_pool.use connection_pool
    (fun dbh ->
       match_lwt
         Lwt_query.query dbh
           <:select< article |
             article in $articles_table$;
             article.id = $int64:id$ >>
       with
         | [] -> Lwt.fail Not_found
         | [article] -> Lwt.return article
         | _ -> Lwt.fail (Failure "Several articles with the same id"))
#endif /* else BASIC_USER */
