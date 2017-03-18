open Sharp_core

open XmlHttpRequest

exception HTTPFailure of http_frame

let frame_receiver   () = Network.event ()
let success_receiver () = Network.event ()
let result_receiver  () = Network.event ()

let diff_receiver () =
  let open Network.Infix in
  (fun success failure -> (success, failure))
  <$> Network.event () <*> Network.event ()

let plug_lwt event value_lwt =
  Lwt.on_success value_lwt (Signal.trigger event)

let plug_lwt_result event value_lwt =
  Lwt.on_any value_lwt (fun value -> Signal.trigger event (Ok value))
                       (fun exn   -> Signal.trigger event (Error exn))

let plug_frame = plug_lwt

let plug_success event frame_lwt =
  Lwt.on_success frame_lwt (fun { code; content } ->
                   if code >= 200 && code < 300
                   then Signal.trigger event content
                   else ()
                 )

let _helper success_event failure_event frame_lwt =
  Lwt.on_any frame_lwt (fun ({ code; content } as frame) ->
               if code >= 200 && code < 300
               then Signal.trigger success_event content
               else Signal.trigger failure_event (HTTPFailure frame)
             ) (Signal.trigger failure_event)

let plug_result event frame_lwt =
  let failure_event = Signal.contramap ~f:(fun err -> Error err) event in
  let success_event = Signal.contramap ~f:(fun res -> Ok    res) event in
  _helper success_event failure_event frame_lwt

let plug_diff success_event failure_event frame_lwt =
  _helper success_event failure_event frame_lwt
