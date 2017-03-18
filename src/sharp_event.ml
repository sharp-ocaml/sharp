open Sharp_core

let make ?(prevent_default=true) event get_value el =
  let connect add =
    let callback el' ev =
      let _ = (match get_value el' ev with
               | Some x -> add x
               | None -> ())
      in not prevent_default
    in
    let listener = Dom_events.listen el event callback in
    fun () -> Dom_events.stop_listen listener
  in
  Network.event ~connect ()

let make_unit ?prevent_default event el =
  make ?prevent_default event (fun _ _ -> Some ()) el

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
