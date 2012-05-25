open Lwt
open Eliom_service
open Eliom_parameter
open Eliom_content.Html5.D

let main_service =
  register_service ~path:["graff"] ~get_params:unit
    (fun () () -> return (html (head (title (pcdata "")) [])
                               (body [h1 [pcdata "Graffiti"]])))

