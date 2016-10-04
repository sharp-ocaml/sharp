open Sharp_core

open XmlHttpRequest

let frame_receiver   () = Network.unbound_event ()
let success_receiver () = Network.unbound_event ()

let diff_receiver () =
  let open Network.Infix in
  (fun success failure -> (success, failure))
  <$> Network.unbound_event () <*> Network.unbound_event ()

let plug_lwt event value_lwt =
  let open Lwt in
  let _ =
    value_lwt >>= fun value ->
    let t = Sys.time () in
    let _ = Behaviour.trigger event t value in
    return ()
  in ()

let plug_frame = plug_lwt

let plug_helper ?failure_event success_event frame_lwt =
  let open Lwt in
  let _ =
    frame_lwt >>= fun ({ code; content } as frame) ->
    let t = Sys.time () in
    let _ = if code >= 200 && code < 300
            then Behaviour.trigger success_event t content
            else match failure_event with
                 | None -> ()
                 | Some event -> Behaviour.trigger event t frame
    in return ()
  in ()

let plug_success event frame_lwt = plug_helper event frame_lwt
let plug_diff success_event failure_event frame_lwt =
  plug_helper ~failure_event success_event frame_lwt
