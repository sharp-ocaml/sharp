open Sharp_core
open Sharp_event

open Behaviour
open Network

let get_value el = Js.to_string el##.value

let text_field el =
  let open Network.Infix in
  input get_value el >>= fun b ->
  return (last ~init:(get_value el) b)

class type validity =
  object
    method valid : bool Js.readonly_prop
  end

class type validElement =
  object
    method validity : validity Js.readonly_prop
    method validationMessage : Js.js_string Js.t Js.readonly_prop
  end

let get_dom_error el =
  let el' = Js.Unsafe.coerce el in
  if el'##.validity##.valid then None else Some (el'##.validationMessage)

let with_dom_error f el =
  let open Network.Infix in
  f el >>= fun bvalue ->
  input get_dom_error el >>= fun berror ->
  let choose value = function
    | Some error -> Error error
    | None       -> Ok value
  in
  return (let open Behaviour.Infix in
          choose <$> bvalue <*> last ~init:None berror)
