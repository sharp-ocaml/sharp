open Js

open Sharp.Core
open Sharp.Event
open Sharp.Form
open Sharp.VDOM

exception Element_not_found of string

let get_element selector =
  Opt.get (Dom_html.document##querySelector (Js.string selector))
          (fun () -> raise (Element_not_found selector))

let () =
  let description_field =
    Opt.get (Dom_html.CoerceTo.input (get_element "#description"))
            (fun () -> assert false)
  in
  let add_button = get_element "#add_item" in
  let data_div   = get_element "#data"     in

  let (remove_event, trigger_remove) = event () in
  let (click_event, _) = click ~prevent_default:true add_button in
  let (description, _) = text_field description_field in

  let add_command = (fun ev descr ->
      match ev with
      | None -> None
      | Some _ -> Some descr
    ) <$> click_event <*> description
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
  let items = fold ~f:step ~init:[] commands in

  let _ =
    vdom data_div items (fun is ->
           tag "ul" |* ("id", "items")
           |+ List.map (fun i ->
                  tag ~id:i "li"
                  |- text i
                  |- (tag ~network:(Sub.click trigger_remove (fun _ -> i))
                          "button"
                      |- text "Remove")
                ) is
         )
  in

  perform commands
          (function
           | Some x, _ -> print_endline ("Add " ^ x)
           | _, Some x -> print_endline ("Delete " ^ x)
           | _ -> ()
          )
