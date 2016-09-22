open Sharp_core

val text_field : Dom_html.inputElement Js.t -> string Behaviour.t Network.t

val with_dom_error :
  ((#Dom_html.element as 'a) Js.t -> 'b Behaviour.t Network.t)
  -> 'a Js.t -> ('b, string) result Behaviour.t Network.t
