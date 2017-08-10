(** Functions to create signals from JS events. *)

open Sharp_core

(** Create a signal from a JS event on a DOM element. *)
val make :
  ?prevent_default:bool
  -> (#Dom_html.event as 'a) Js.t Dom.Event.typ
  -> ((#Dom_html.eventTarget as 'b) Js.t -> 'a Js.t -> 'c option) -> 'b Js.t
  -> 'c option t * (unit -> unit)
(** The callback receives the event target and the event itself and returns
    a value only if the signal should take a value.
    It returns the signal as well as a function which unbinds the signal when
    called.
 *)

(** A convenient specialised version of [make] for signals that always take the
    unit value. *)
val make_unit :
  ?prevent_default:bool
  -> #Dom_html.event Js.t Dom.Event.typ -> #Dom_html.eventTarget Js.t
  -> unit option t * (unit -> unit)

val click : ?prevent_default:bool -> #Dom_html.eventTarget Js.t
            -> unit option t * (unit -> unit)
val submit : ?prevent_default:bool -> #Dom_html.eventTarget Js.t
             -> unit option t * (unit -> unit)
val change : ?prevent_default:bool -> ((#Dom_html.eventTarget as 'a) Js.t
                                       -> Dom_html.event Js.t -> 'b option)
             -> 'a Js.t -> 'b option t * (unit -> unit)
val input : ?prevent_default:bool -> ((#Dom_html.eventTarget as 'a) Js.t
                                      -> Dom_html.event Js.t -> 'b option)
            -> 'a Js.t -> 'b option t * (unit -> unit)
val hashchange : ?prevent_default:bool -> (Dom_html.window Js.t
                                           -> Dom_html.hashChangeEvent Js.t
                                           -> 'a option)
                 -> 'a option t * (unit -> unit)
val popstate : ?prevent_default:bool -> (Dom_html.window Js.t
                                         -> Dom_html.popStateEvent Js.t
                                         -> 'a option)
               -> 'a option t * (unit -> unit)
