
open Eliom_parameter

#ifndef MINIMAL_PROJECT
module ##MODULE_NAME##_appl =
  Eliom_registration.App (struct
    let application_name = "##PROJECT_NAME##"
  end)
#endif /* MINIMAL_PROJECT */
let main_service =
  Eliom_service.service
    ~path:[]
    ~get_params:unit
    ()
#ifdef BASIC_USER
let connect_service =
  Eliom_service.post_coservice'
    ~name:"login"
    ~post_params:(string "login" ** string "password")
    ()

let signout_service =
  Eliom_service.post_coservice'
    ~name:"signout"
    ~post_params:unit
    ()

let important_service =
  Eliom_service.service
    ~path:["important"]
    ~get_params:unit
    ()
#endif /* BASIC_USER */
