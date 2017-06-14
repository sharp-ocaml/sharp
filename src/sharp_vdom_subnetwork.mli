open Sharp_core

val click : ?prevent_default:bool -> ('a -> unit)
            -> ((#Dom_html.eventTarget as 'b) Js.t -> 'a) -> 'b Js.t
            -> (unit -> unit)

val input : ('a -> unit) -> ((#Dom_html.eventTarget as 'b) Js.t -> 'a)
            -> 'b Js.t -> (unit -> unit)

val text_field : (string -> unit) -> #Sharp_form.field Js.t -> (unit -> unit)

val submit : ?prevent_default:bool -> (unit -> unit)
             -> #Dom_html.eventTarget Js.t -> (unit -> unit)

val none : 'a Js.t -> (unit -> unit)
