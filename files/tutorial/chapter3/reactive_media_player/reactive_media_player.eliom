{shared{
  open Eliom_content
  open Html5


  type action = Play | Pause | Seek of float deriving (Json)

  let string_of_action = function
    | Play -> "play"
    | Pause -> "pause"
    | Seek f -> Format.sprintf "seek %f" f

}}


{client{

  let media_s, set_media_s = React.S.create Pause
  let progress_s, set_progress_s = React.S.create (0., 0.)
  let unblock_s, set_unblock_s = React.S.create true


  let oninput_emit input _ _ =
    set_media_s (Seek (Js.parseFloat (input##value)));
    Lwt.return ()


  let ontimeupdate_emit media _ _ =
    set_progress_s (media##currentTime, media##duration);
    Lwt.return ()

}}

let progress_bar () =
  let progress_value = {float React.signal{ React.S.map (fun (time, duration) ->
    if duration = 0. then 0. else time /. duration *. 100.) progress_s }} in
  let attrs = D.([
    a_input_min 0.;
    a_input_max 100.;
    a_onmousedown {{fun _ -> set_unblock_s false}};
    a_onmouseup {{fun _ -> set_unblock_s true}};
    C.attr {{R.a_value (React.S.map (Format.sprintf "%0.f")
                          (React.S.on unblock_s 0. %progress_value))}};])
  in
  let d_input = D.(float_input ~input_type:`Range () ~value:0. ~a:attrs) in
  let _ = {unit{
    Lwt.async (fun () ->
      let d_input = To_dom.of_input %d_input in
      Lwt_js_events.inputs d_input (oninput_emit d_input)
    )}}
  in d_input

let media_uri = (Html5.D.make_uri
              ~service:(Eliom_service.static_dir ())
              ["hb.mp3"])

let exemple_div () =
  C.node {{R.node (React.S.map (
    fun s_value -> D.div [D.pcdata (string_of_action s_value)]
  ) media_s)}}


let media_tag () =
  let media = D.(audio ~src:(media_uri)[pcdata "alt"]) in
  let _ = {unit{
    Lwt.async (fun () ->
      let media = To_dom.of_audio %media in
      Lwt_react.S.keep (React.S.map (function
        | Play -> media##play ()
        | Pause -> media##pause ()
        | Seek f -> media##currentTime <- (f /. 100. *. media##duration))
        media_s);
      Lwt_js_events.timeupdates media (ontimeupdate_emit media);
      )}}
  in media


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
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()


let () =
  React_Player_app.register
    ~service:media_service
    (fun name () ->
      Lwt.return D.(
        Eliom_tools.D.html ~title:"Media" ~css:[]
          (body [
            h2 [pcdata "Media"];
            media_tag (); exemple_div ();
            div [play_button (); pause_button (); progress_bar ()];])))
