open Dom_html

open Sharp_core

type time_diff = time

type 'a command =
  | Once   of 'a * time_diff
  | Every  of 'a * time_diff
  | NTimes of 'a * time_diff * int

type 'a spec =
  | SpecOnce   of 'a * time
  | SpecEvery  of 'a * time * time_diff
  | SpecNTimes of 'a * time * time_diff * int

let command_to_spec t command =
  match command with
  | Once   (i, d)    -> SpecOnce   (i, t +. d)
  | Every  (i, d)    -> SpecEvery  (i, t +. d, d)
  | NTimes (i, d, n) -> SpecNTimes (i, t +. d, d, n)

let extract_time = function
  | SpecOnce   (_, t)       -> t
  | SpecEvery  (_, t, _)    -> t
  | SpecNTimes (_, t, _, _) -> t

let extract_value = function
  | SpecOnce   (v, _)       -> v
  | SpecEvery  (v, _, _)    -> v
  | SpecNTimes (v, _, _, _) -> v

let next_spec = function
  | SpecOnce   (v, _)    -> None
  | SpecEvery  (v, t, d) -> Some (command_to_spec t (Every (v, d)))
  | SpecNTimes (v, _, d, n) when n <= 1 -> None
  | SpecNTimes (v, t, d, n) ->
     Some (command_to_spec t (NTimes (v, d, n-1)))

let sort_specs specs =
  List.fast_sort (fun x y ->
      let tx = extract_time x in
      let ty = extract_time y in
      let diff = tx -. ty in
      (* we can't just round the float since it would cause errors around 0 *)
      if diff > 0. then 1 else if diff < 0. then -1 else 0
    ) specs

let add_to_specs specs spec = sort_specs (spec :: specs)

let remove_from_specs specs spec =
  List.filter (fun spec' -> spec != spec') specs

let rec plan_next_tick now tidref sref event =
  match !sref with
  | [] -> ()
  | spec :: _ ->
     let t    = extract_time spec in
     let diff = max epsilon_float (t -. now) in (* treat ticks in the past *)
     let tid  = setTimeout (fun () ->
                    let specs = remove_from_specs (!sref) spec in
                    let specs' = match next_spec spec with
                      | None       -> specs
                      | Some spec' -> add_to_specs specs spec'
                    in
                    sref := specs';

                    tidref := None;
                    plan_next_tick (Sys.time ()) tidref sref event;
                    let v = extract_value spec in
                    let _ = Behaviour.trigger event v in ()
                  ) (1000. *. diff)
     in tidref := Some tid

let interrupt tidref =
  match !tidref with
  | None     -> ()
  | Some tid -> clearTimeout tid; tidref := None

let tick_manager commands =
  let specs  = sort_specs (List.map (command_to_spec (Sys.time ())) commands) in
  let sref   = ref specs in
  let tidref = ref None in

  let open Network.Infix in
  Network.unbound_event () >>= fun tick_event ->

  let connect trigger =
    plan_next_tick (Sys.time ()) tidref sref tick_event;
    fun () -> interrupt tidref
  in
  Network.event connect >>= fun command_event ->

  Network.react command_event Behaviour.time (fun cmd t ->
                  interrupt tidref;
                  sref := add_to_specs (!sref) (command_to_spec t cmd);
                  plan_next_tick t tidref sref tick_event
                )
  >> return (Behaviour.combine tick_event command_event)

let every diff value =
  Network.map ~f:Behaviour.no_trigger (tick_manager [Every (value, diff)])

let last_for diff { Behaviour.behaviour; trigger } =
  let rec f b current now =
    let (opt, b') = Behaviour.at b now in
    match opt, current with
    | None, Some (x, past) when past +. diff >= now ->
       (Some x, Behaviour.B (f b' current))
    | None, _ -> (None, Behaviour.B (f b' None))
    | Some x, _ -> (Some x, Behaviour.B (f b' (Some (x, now))))
  in { Behaviour.behaviour = Behaviour.B (f behaviour None); trigger }
