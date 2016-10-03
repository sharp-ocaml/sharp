open Sharp_core

open Behaviour
open Network

let click ?prevent_default ?value ?get_value event node =
  let trigger' = match value, get_value with
    | Some x, _ -> fun t -> trigger event t x
    | _, Some f -> fun t -> trigger event t (f ())
    | _ -> assert false
  in
  let open Network.Infix in
  Sharp_event.click ?prevent_default node >>= fun event' ->
  let open Behaviour.Infix in
  react_ ((fun x y -> (x, y)) <$> time <*> to_behaviour event')
         (function | _, None    -> ()
                   | t, Some () -> trigger' t)
