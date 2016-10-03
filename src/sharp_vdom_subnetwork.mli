open Sharp_core

val click : ?prevent_default:bool -> ?value:'a -> ?get_value:(unit -> 'a)
            -> 'a Behaviour.event -> #Dom_html.eventTarget Js.t
            -> unit Network.t
