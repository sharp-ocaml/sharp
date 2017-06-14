open Sharp_core

class type field = object
  inherit Dom_html.element
  method value : Js.js_string Js.t Js.prop
end

val text_field : #field Js.t -> string t * (unit -> unit)

val with_dom_error :
  ((#Dom_html.element as 'a) Js.t -> 'b t * (unit -> unit))
  -> 'a Js.t -> ('b, string) result t * (unit -> unit)
