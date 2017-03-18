open Sharp_core

open Signal
open Network

let click ?prevent_default event get_value node =
  let open Network.Infix in
  Sharp_event.click ?prevent_default node >>= fun event' ->
  react event' (Signal.pure ()) (fun _ _ -> trigger event (get_value node))

let input event get_value node =
  let open Network.Infix in
  Sharp_event.input (fun el _ -> Some (get_value el)) node >>= fun event' ->
  react event' (Signal.pure ()) (fun x _ -> trigger event x)

let text_field event node =
  let open Network.Infix in
  input event (fun _ -> Js.to_string (node##.value)) node
  >> initially (fun _ -> trigger event (Js.to_string (node##.value)))

let submit ?prevent_default event node =
  let open Network.Infix in
  Sharp_event.submit ?prevent_default node >>= fun event' ->
  react event' (Signal.pure ()) (fun _ _ -> trigger event ())

let none _ = return ()
