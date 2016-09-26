open Sharp_core

let make ?(prevent_default=false) event get_value el =
  let f add =
    let callback _ _ = add (get_value el); not prevent_default in
    let listener = Dom_events.listen el event callback in
    fun () -> Dom_events.stop_listen listener
  in
  Network.event f

let make_unit ?prevent_default event el =
  make ?prevent_default event (fun _ -> ()) el

let with_opt ?(descr="unknown") ~default f el_opt =
  Js.Opt.case el_opt
              (fun () ->
                print_string "with_opt: Couldn't bind to an element: ";
                print_endline descr;
                Network.return (Behaviour.pure default)
              ) f

let ( <% ) f el_opt = with_opt ~default:None f el_opt
let ( %> ) el_opt f = f <% el_opt

let click ?prevent_default el =
  make_unit ?prevent_default Dom_html.Event.click el
let change ?prevent_default f el =
  make ?prevent_default Dom_html.Event.change f el
let input ?prevent_default f el =
  make ?prevent_default Dom_html.Event.input f el