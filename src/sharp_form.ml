open Sharp_core
open Sharp_event

let get_value (el : Dom_html.inputElement Js.t) = Js.to_string el##.value

let input el =
  Network.map (change get_value el)
              ~f:(Behaviour.last ~init:(get_value el))
