open Js

open Sharp.Core.Behaviour
open Sharp.Core.Network
open Sharp.Core
open Sharp.Event
open Sharp.Form
open Sharp.VDOM

exception Element_not_found of string

let get_element selector =
  Opt.get (Dom_html.document##querySelector (Js.string selector))
          (fun () -> raise (Element_not_found selector))

let item_network callback i btn =
  let open Network.Infix in
  click btn >>= fun signal ->
  let open Behaviour.Infix in
  let timed_signal = (fun x y -> (x, y)) <$> time <*> signal in
  react_ timed_signal
         (fun (t, opt) -> match opt with
                          | None -> ()
                          | Some _ -> callback t i
         )

let network () =
  let description_field =
    Opt.get (Dom_html.CoerceTo.input (get_element "#description"))
            (fun () -> assert false)
  in
  let add_button = get_element "#add_item" in
  let data_div   = get_element "#data"     in

  let open Network.Infix in
  text_field description_field >>= fun description ->
  click ~prevent_default:true add_button >>= fun add_click ->

  unbound_event () >>= fun ((remove_command : string option Behaviour.t)
                           , remove_callback) ->
  let open Behaviour.Infix in
  let (add_command : string option Behaviour.t) =
    (fun x y -> match x with | None -> None | Some _ -> Some y)
    <$> add_click <*> description
  in
  let commands = (fun x y -> (x, y)) <$> add_command <*> remove_command in

  let step is (add_opt, remove_opt) =
    let is' = match add_opt with
      | None -> is
      | Some item -> is @ [item]
    in
    match remove_opt with
    | None -> is'
    | Some item -> List.filter (fun x -> x != item) is'
  in
  let items = fold step [] commands in

  vdom data_div items (fun is ->
         tag "ul" |> set_attribute "id" "items"
         |+ List.map (fun i ->
             tag "li"
             |- text i
             |- (tag ~network:(item_network remove_callback i) "button"
                 |- text "Remove")
           ) is
       )

let () =
  let _ = start (network ()) in ()
