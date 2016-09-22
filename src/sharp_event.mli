open Sharp_core

val make :
  ?prevent_default:bool
  -> #Dom_html.event Js.t Dom.Event.typ
  -> ((#Dom_html.eventTarget as 'a) Js.t -> 'b) -> 'a Js.t
  -> 'b option Behaviour.t Network.t

val make_unit :
  ?prevent_default:bool
  ->   #Dom_html.event Js.t Dom.Event.typ -> #Dom_html.eventTarget Js.t
  -> unit option Behaviour.t Network.t

val with_opt :
  ?descr:string -> default:'a -> ('b -> 'a Behaviour.t Network.t) -> 'b Js.opt
  -> 'a Behaviour.t Network.t

val ( <% ) : ('a -> 'b option Behaviour.t Network.t) -> 'a Js.opt
             -> 'b option Behaviour.t Network.t
val ( %> ) : 'a Js.opt -> ('a -> 'b option Behaviour.t Network.t)
             -> 'b option Behaviour.t Network.t

val click : ?prevent_default:bool ->
            #Dom_html.eventTarget Js.t -> unit option Behaviour.t Network.t
val change : ?prevent_default:bool -> ((#Dom_html.eventTarget as 'a) Js.t -> 'b)
             -> 'a Js.t -> 'b option Behaviour.t Network.t
val input : ?prevent_default:bool -> ((#Dom_html.eventTarget as 'a) Js.t -> 'b)
            -> 'a Js.t -> 'b option Behaviour.t Network.t
