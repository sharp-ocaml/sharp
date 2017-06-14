open Sharp_core
open Sharp_event

class type field = object
  inherit Dom_html.element
  method value : Js.js_string Js.t Js.prop
end

let get_value el = Js.to_string el##.value

let text_field el =
  let (signal, stop) = input (fun _ _ -> Some (get_value el)) el in
  (last ~init:(get_value el) signal, stop)

class type validity =
  object
    method valid : bool Js.readonly_prop
  end

class type validElement =
  object
    method validity : validity Js.readonly_prop
    method validationMessage : Js.js_string Js.t Js.readonly_prop
  end

let get_dom_error el _ =
  let el' = Js.Unsafe.coerce el in
  if el'##.validity##.valid then None else Some (el'##.validationMessage)

let with_dom_error f el =
  let (svalue, stop) = f el in
  let (error_event, _) = input get_dom_error el in
  let choose value = function
    | Some error -> Error error
    | None       -> Ok value
  in
  let signal = choose <$> svalue <*> last ~init:None error_event in
  (signal, stop)
