open Sharp_core
open Sharp_event

open Signal
open Network

class type field = object
  inherit Dom_html.element
  method value : Js.js_string Js.t Js.prop
end

let get_value el = Js.to_string el##.value

let text_field el =
  let open Network.Infix in
  input (fun _ _ -> Some (get_value el)) el >>= fun ev ->
  return (last ~init:(get_value el) ev)

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
  let open Network.Infix in
  f el >>= fun svalue ->
  input get_dom_error el >>= fun error_event ->
  let choose value = function
    | Some error -> Error error
    | None       -> Ok value
  in
  let open Signal.Infix in
  let signal  = choose <$> svalue <*> last ~init:None error_event in
  let signal' = combine svalue signal in
  Network.return signal'
