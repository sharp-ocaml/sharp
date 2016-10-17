open Sharp_core

val click : ?prevent_default:bool -> 'a Behaviour.event
            -> ((#Dom_html.eventTarget as 'b) Js.t -> 'a) -> 'b Js.t
            -> unit Network.t

val input : 'a Behaviour.event
            -> ((#Dom_html.eventTarget as 'b) Js.t -> 'a) -> 'b Js.t
            -> unit Network.t

val text_field : string Behaviour.event -> #Sharp_form.field Js.t
                 -> unit Network.t

val submit : ?prevent_default:bool -> unit Behaviour.event
             -> #Dom_html.eventTarget Js.t -> unit Network.t

val none : 'a Js.t -> unit Network.t
