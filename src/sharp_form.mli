open Sharp_category
open Sharp_core

class type field = object
  inherit Dom_html.element
  method value : Js.js_string Js.t Js.prop
end

val text_field : #field Js.t -> (string, string) Signal.t Network.t

val with_dom_error :
  ((#Dom_html.element as 'a) Js.t -> ('b, 'c) Signal.t Network.t)
  -> 'a Js.t -> ('b, ('c, string) result) Signal.t Network.t
