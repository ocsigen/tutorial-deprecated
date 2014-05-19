
{shared{
  open Eliom_content
  open Html5
  open Format
}}


{client{

  type action = Play | Pause | Seek of float deriving (Json)

  let media_s, set_media_s = React.S.create Pause
  let progress_s, set_progress_s = React.S.create (0., 0.)
  let unblock_s, set_unblock_s = React.S.create true

  let get_target ev =
    Js.Unsafe.coerce (Js.Opt.get (ev##target) (fun _ -> assert false))

}}

let progress_bar () =
  let progress_value = {float React.signal{ React.S.map (fun (time, duration) ->
    if duration = 0. then 0. else time /. duration *. 100.)
    progress_s }} in
  let oninput = {{ fun ev ->
    let target : Dom_html.inputElement Js.t = get_target ev in
    let input_value = Js.parseFloat (target##value) in
    set_media_s (Seek input_value) }} in
  let attrs = D.([a_input_min 0.; a_input_max 100.; a_oninput oninput;
                  a_onmousedown {{fun _ -> set_unblock_s false}};
                  a_onmouseup {{fun _ -> set_unblock_s true}};
                  C.attr {{R.a_value (React.S.map (sprintf "%0.f")
                               (React.S.on unblock_s 0. %progress_value))}};])
  in D.(float_input ~input_type:`Range () ~value:0. ~a:attrs)


let media_uri = (Html5.D.make_uri
              ~service:(Eliom_service.static_dir ())
              ["sin.webm"])


let media_tag () =
  let timeupdate = {{fun ev ->
    let media : Dom_html.videoElement Js.t = get_target ev in
        set_progress_s (media##currentTime, media##duration);
  }} in
  let media = D.(audio
                   ~a:[a_ontimeupdate timeupdate]
                   ~src:(media_uri)[pcdata "alt"]) in
  let _ = {unit{
    Lwt_js_events.async (fun () ->
      let media = To_dom.of_video %media in
      let _ = React.S.map (function
        | Play -> media##play ()
        | Pause -> media##pause ()
        | Seek f -> media##currentTime <- (f /. 100. *. media##duration))
        media_s in
      Lwt.return ())
  }} in
  media

let pause_button () =
  D.(string_input ~input_type:`Submit () ~value:"Pause"
       ~a:[a_onclick {{ fun _ -> set_media_s Pause }}])

let play_button () =
  D.(string_input ~input_type:`Submit () ~value:"Play"
       ~a:[a_onclick {{ fun _ -> set_media_s Play }}])

module React_Player_app =
  Eliom_registration.App (
    struct
      let application_name = "react_player"
    end)


let media_service =
  Eliom_service.App.service ~path:[""] ~get_params:Eliom_parameter.unit ()



(* ; css_link ~uri:css_uri () *)
let () =
  React_Player_app.register
    ~service:media_service
    (fun name () ->
      Lwt.return D.(
        Eliom_tools.D.html ~title:"Media" ~css:[]
          (body [
            h2 [pcdata "Media"];
            media_tag ();
            div [play_button (); pause_button (); progress_bar ()];])))
