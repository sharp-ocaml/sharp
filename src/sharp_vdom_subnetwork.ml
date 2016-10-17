open Sharp_core

open Behaviour
open Network

let click ?prevent_default event get_value node =
  let open Network.Infix in
  Sharp_event.click ?prevent_default node >>= fun event' ->
  react event' (Behaviour.pure ()) (fun _ _ -> trigger event (get_value node))

let input event get_value node =
  let open Network.Infix in
  Sharp_event.input get_value node >>= fun event' ->
  react event' (Behaviour.pure ()) (fun x _ -> trigger event x)

let text_field event node =
  input event (fun _ -> Js.to_string (node##.value)) node

let submit ?prevent_default event node =
  let open Network.Infix in
  Sharp_event.submit ?prevent_default node >>= fun event' ->
  react event' (Behaviour.pure ()) (fun _ _ -> trigger event ())

let none _ = return ()
