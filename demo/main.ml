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

let network () =
  let description_field =
    Opt.get (Dom_html.CoerceTo.input (get_element "#description"))
            (fun () -> assert false)
  in
  let add_button = get_element "#add_item" in
  let data_div   = get_element "#data"     in

  let open Network.Infix in
  text_field description_field >>= fun description ->
  click ~prevent_default:true add_button >>= fun click_event ->
  event () >>= fun remove_event ->

  let open Behaviour.Infix in
  let add_command =
    (fun x y -> match x with | None -> None | Some _ -> Some y)
    <$> click_event <*> description
  in
  let commands = (fun x y -> (x, y)) <$> add_command <*> remove_event in

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
         tag "ul" |* ("id", "items")
         |+ List.map (fun i ->
             tag "li"
             |- text i
             |- (tag ~network:(Sub.click remove_event (fun _ -> i)) "button"
                 |- text "Remove")
           ) is
       )

let () =
  let _ = start (network ()) in ()
