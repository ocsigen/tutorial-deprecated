
#ifdef WITH_DATABASE
let db_name = ref ""
let db_user = ref ""

let () =
  Eliom_config.parse_config
    Ocsigen_extensions.Configuration.([
      element ~name:"database"
        ~attributes:[
          attribute ~name:"name" ~obligatory:true
            (fun name -> db_name := name);
          attribute ~name:"user" ~obligatory:true
            (fun user -> db_user := user);
        ]
        ();
    ])
#endif /* WITH_DATABASE */
