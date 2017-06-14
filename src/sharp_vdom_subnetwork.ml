open Sharp_core

let click ?prevent_default trigger get_value node =
  let (event, stop) = Sharp_event.click ?prevent_default node in
  react event (fun _ -> trigger (get_value node));
  stop

let input trigger get_value node =
  let (event, stop) = Sharp_event.input (fun el _ ->
                          Some (get_value el)) node in
  react event trigger;
  stop

let text_field trigger node =
  let stop = input trigger (fun _ -> Js.to_string (node##.value)) node in
  trigger (Js.to_string node##.value);
  stop

let submit ?prevent_default trigger node =
  let (event, stop) = Sharp_event.submit ?prevent_default node in
  react event trigger;
  stop

let none _ () = ()
