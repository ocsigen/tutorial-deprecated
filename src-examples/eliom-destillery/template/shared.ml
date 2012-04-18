
let map_option_lwt f v =
  match v with
    | None -> Lwt.return None
    | Some a -> f a >>= fun r -> Lwt.return (Some r)

