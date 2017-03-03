open Sharp_core

open Behaviour
open Network

let click ?prevent_default event get_value node =
  let open Network.Infix in
  Sharp_event.click ?prevent_default node >>= fun event' ->
  react event' (Behaviour.pure ()) (fun _ _ ->
          let _ = trigger event (get_value node) in ()
        )

let input event get_value node =
  let open Network.Infix in
  Sharp_event.input (fun el _ -> Some (get_value el)) node >>= fun event' ->
  react event' (Behaviour.pure ()) (fun x _ ->
          let _ = trigger event x in ()
        )

let text_field event node =
  let open Network.Infix in
  input event (fun _ -> Js.to_string (node##.value)) node
  >> initially (fun _ ->
         let _ = trigger event (Js.to_string (node##.value)) in ()
       )

let submit ?prevent_default event node =
  let open Network.Infix in
  Sharp_event.submit ?prevent_default node >>= fun event' ->
  react event' (Behaviour.pure ()) (fun _ _ ->
          let _ = trigger event () in ()
        )

let none _ = return ()
