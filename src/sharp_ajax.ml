open Sharp_core

open XmlHttpRequest

let ajax_receiver () = Network.unbound_event ()

let plug_lwt callback mframe =
  let open Lwt in
  let now = Sys.time () in
  let _ =
    mframe >>= fun frame ->
    let _ = callback now frame in
    return ()
  in ()

let plug_ajax_200 callback mframe =
  let callback' time { code; content } =
    if code == 200 then callback time content else ()
  in
  plug_lwt callback' mframe
