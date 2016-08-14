open Sharp_core

let mk event get_value el =
  let open Network in
  let (ev, add) = Behaviour.event () in

  let connect signal =
    let callback _ _ =
      let t = Sys.time () in
      add t (get_value el); signal t; true
    in
    let listener = Dom_events.listen el event callback in
    fun () -> Dom_events.stop_listen listener
  in

  add_funnel connect >> return ev

let mk_unit event el = mk event (fun _ -> ()) el

let with_opt ?(descr="unknown") ~default f el_opt =
  Js.Opt.case el_opt
              (fun () ->
                print_string "Couldn't bind event listener to an element: ";
                print_endline descr;
                Network.return (Behaviour.const default)
              ) f

let ( <* ) f el_opt = with_opt ~default:None f el_opt
let ( *> ) el_opt f = f <* el_opt

let click el = mk_unit Dom_html.Event.click el
let change f el = mk Dom_html.Event.change f el
