open Sharp_core

val make :
  ?prevent_default:bool
  -> #Dom_html.event Js.t Dom.Event.typ
  -> ((#Dom_html.eventTarget as 'a) Js.t -> 'b) -> 'a Js.t
  -> 'b Behaviour.event Network.t

val make_unit :
  ?prevent_default:bool
  -> #Dom_html.event Js.t Dom.Event.typ -> #Dom_html.eventTarget Js.t
  -> unit Behaviour.event Network.t

val with_opt :
  ?descr:string -> ?default:'a -> ('b -> 'a Behaviour.event Network.t)
  -> 'b Js.opt -> 'a Behaviour.event Network.t

val ( <% ) : ('a -> 'b Behaviour.event Network.t) -> 'a Js.opt
             -> 'b Behaviour.event Network.t
val ( %> ) : 'a Js.opt -> ('a -> 'b Behaviour.event Network.t)
             -> 'b Behaviour.event Network.t

val click : ?prevent_default:bool ->
            #Dom_html.eventTarget Js.t -> unit Behaviour.event Network.t
val submit : ?prevent_default:bool ->
             #Dom_html.eventTarget Js.t -> unit Behaviour.event Network.t
val change : ?prevent_default:bool -> ((#Dom_html.eventTarget as 'a) Js.t -> 'b)
             -> 'a Js.t -> 'b Behaviour.event Network.t
val input : ?prevent_default:bool -> ((#Dom_html.eventTarget as 'a) Js.t -> 'b)
            -> 'a Js.t -> 'b Behaviour.event Network.t
val hashchange : ?prevent_default:bool -> (Dom_html.window Js.t -> 'b)
                 -> 'b Behaviour.event Network.t
