open Sharp_core

val click : ?prevent_default:bool -> ('a, 'b) Signal.t
            -> ((#Dom_html.eventTarget as 'c) Js.t -> 'a) -> 'c Js.t
            -> unit Network.t

val input : ('a, 'b) Signal.t
            -> ((#Dom_html.eventTarget as 'c) Js.t -> 'a) -> 'c Js.t
            -> unit Network.t

val text_field : (string, 'a) Signal.t -> #Sharp_form.field Js.t
                 -> unit Network.t

val submit : ?prevent_default:bool -> (unit, 'a) Signal.t
             -> #Dom_html.eventTarget Js.t -> unit Network.t

val none : 'a Js.t -> unit Network.t
