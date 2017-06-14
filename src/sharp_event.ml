open Sharp_core

let make ?(prevent_default=true) dom_event get_value el =
  let (ev, trigger) = event () in
  let callback el' ev =
    let _ = (match get_value el' ev with
             | Some x -> trigger x
             | None -> ())
    in not prevent_default
  in
  let listener = Dom_events.listen el dom_event callback in
  let stop () = Dom_events.stop_listen listener
  in (ev, stop)

let make_unit ?prevent_default dom_event el =
  make ?prevent_default dom_event (fun _ _ -> Some ()) el

let click ?prevent_default el =
  make_unit ?prevent_default Dom_html.Event.click el
let submit ?prevent_default el =
  make_unit ?prevent_default Dom_html.Event.submit el
let change ?prevent_default f el =
  make ?prevent_default Dom_html.Event.change f el
let input ?prevent_default f el =
  make ?prevent_default Dom_html.Event.input f el
let hashchange ?prevent_default f =
  make ?prevent_default Dom_html.Event.hashchange f Dom_html.window
let popstate ?prevent_default f =
  make ?prevent_default (Dom_html.Event.make "popstate") f Dom_html.window
