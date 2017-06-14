open Sharp_core

open XmlHttpRequest

exception HTTPFailure of http_frame

let frame_receiver   = event
let success_receiver = event
let result_receiver  = event
let diff_receiver () = (event (), event ())

let plug_lwt trigger value_lwt = Lwt.on_success value_lwt trigger
let plug_lwt_result trigger value_lwt =
  Lwt.on_any value_lwt (fun value -> trigger (Ok value))
                       (fun exn   -> trigger (Error exn))

let plug_frame = plug_lwt

let plug_success trigger frame_lwt =
  Lwt.on_success frame_lwt (fun { code; content } ->
                   if code >= 200 && code < 300
                   then trigger content
                   else ()
                 )

let _helper success_trigger failure_trigger frame_lwt =
  Lwt.on_any frame_lwt (fun ({ code; content } as frame) ->
               if code >= 200 && code < 300
               then success_trigger content
               else failure_trigger (HTTPFailure frame)
             ) failure_trigger

let plug_result trigger frame_lwt =
  let success_trigger res = trigger (Ok res) in
  let failure_trigger err = trigger (Error err) in
  _helper success_trigger failure_trigger frame_lwt

let plug_diff success_trigger failure_trigger frame_lwt =
  _helper success_trigger failure_trigger frame_lwt
