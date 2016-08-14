open Js

open Sharp.Core.Behaviour
open Sharp.Core.Network
open Sharp.Event
open Sharp.Form

let turn_paragraph_blue () =
  Opt.iter
    (Dom_html.document##querySelector (Js.string "#paragraph"))
    (fun p -> p##.style##.color := Js.string "blue")

let () =
  let btn_opt = Dom_html.document##querySelector (Js.string "#blue_button") in
  let field_opt =
    Js.Opt.bind (Dom_html.document##querySelector (Js.string "#field"))
                Dom_html.CoerceTo.input
  in

  let network =
    click <* btn_opt >>= fun c ->
    with_opt ~default:"" input field_opt >>= fun value ->

    let click_count = count c in
    let both = lift2 click_count value ~f:(fun x y -> (x, y)) in

    react both (fun (count, value) ->
            if count >= 3
            then turn_paragraph_blue ()
            else print_endline value
          )
  in
  let _ = start network in ()
