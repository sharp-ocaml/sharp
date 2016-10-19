open Sharp_core

open XmlHttpRequest

exception HTTPFailure of http_frame

let frame_receiver   () = Network.unbound_event ()
let success_receiver () = Network.unbound_event ()
let result_receiver  () = Network.unbound_event ()

let diff_receiver () =
  let open Network.Infix in
  (fun success failure -> (success, failure))
  <$> Network.unbound_event () <*> Network.unbound_event ()

let plug_lwt event value_lwt =
  Lwt.on_success value_lwt (fun value ->
                   let _ = Behaviour.trigger event value in ()
                 )

let plug_lwt_result event value_lwt =
  Lwt.on_any value_lwt (fun value ->
               let _ = Behaviour.trigger event (Ok value) in ()
             ) (fun exn ->
               let _ = Behaviour.trigger event (Error exn) in ()
             )

let plug_frame = plug_lwt

let plug_success event frame_lwt =
  Lwt.on_success frame_lwt (fun { code; content } ->
                   let _ = if code >= 200 && code < 300
                           then Behaviour.trigger event content
                           else None
                   in ()
                 )

let _helper success_event failure_event frame_lwt =
  Lwt.on_any frame_lwt (fun ({ code; content } as frame) ->
               let _ = if code >= 200 && code < 300
                       then Behaviour.trigger success_event content
                       else Behaviour.trigger failure_event (HTTPFailure frame)
               in ()
             ) (fun exn -> let _ = Behaviour.trigger failure_event exn in ())

let plug_result event frame_lwt =
  let failure_event = Behaviour.contramap ~f:(fun err -> Error err) event in
  let success_event = Behaviour.contramap ~f:(fun res -> Ok    res) event in
  _helper success_event failure_event frame_lwt
let plug_diff success_event failure_event frame_lwt =
  _helper success_event failure_event frame_lwt
