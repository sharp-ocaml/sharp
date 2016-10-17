open Sharp_core

val click : ?prevent_default:bool -> ('a, 'b) Behaviour.t
            -> ((#Dom_html.eventTarget as 'c) Js.t -> 'b) -> 'c Js.t
            -> unit Network.t

val input : ('a, 'b) Behaviour.t
            -> ((#Dom_html.eventTarget as 'c) Js.t -> 'b) -> 'c Js.t
            -> unit Network.t

val text_field : ('a, string) Behaviour.t -> #Sharp_form.field Js.t
                 -> unit Network.t

val submit : ?prevent_default:bool -> ('a, unit) Behaviour.t
             -> #Dom_html.eventTarget Js.t -> unit Network.t

val none : 'a Js.t -> unit Network.t
