type time = float

module Event = struct
  type 'a t =
    | Cons of time * 'a * 'a t
    | Stop of 'a t option ref

  let create () = Stop (ref None)

  let rec add es t x = match es with
    | Cons (t', _, es') when t != t' -> add es' t x
    | Cons (t', _, es') -> add es' (t +. epsilon_float) x
    | Stop r ->
       match !r with
       | None ->
          let tip = create () in
          r := Some (Cons (t, x, tip)); (t, tip)
       | Some es' -> add es' t x

  let rec at es t = match es with
    | Cons (t', x, _) when t = t' -> Some x
    | Cons (t', _, _) when t < t' -> None
    | Cons (_, _, es') -> at es' t
    | Stop r ->
       match !r with
       | None     -> None
       | Some es' -> at es' t

  let rec after es t = match es with
    | Cons (t', _, es') when t >= t' -> after es' t
    | Cons (_, _, _) -> es
    | Stop r ->
       match !r with
       | None     -> es
       | Some es' -> after es' t
end

type 'a t =
  { timed_value  : time -> 'a * 'a t
  ; propagateref : (time -> unit) ref
  }

let at { timed_value } t = timed_value t

let mkpure f =
  let propagateref = ref (fun _ -> ()) in
  let rec s = { timed_value = (fun t -> f s t); propagateref } in
  s

let register { propagateref } f =
  let propagate = !propagateref in
  propagateref := (fun t -> propagate t; f t)

let const x = mkpure (fun s _ -> (x, s))
let time = mkpure (fun s t -> (t, s))

let rec map f sx =
  let propagateref = ref (fun _ -> ()) in
  let timed_value t =
    let (x, sx') = at sx t in
    (f x, map f sx')
  in
  register sx (fun t -> !propagateref t);
  { timed_value; propagateref }

let ( <$> ) = map

let rec apply sf sx =
  let propagateref = ref (fun _ -> ()) in
  let timed_value t =
    let (f, sf') = at sf t in
    let (x, sx') = at sx t in
    (f x, apply sf' sx')
  in
  register sf (fun t -> !propagateref t);
  register sx (fun t -> !propagateref t);
  { timed_value; propagateref }

let ( <*> ) = apply
let pure = const
let lift0 = const
let lift = map
let lift2 f sa sb = f <$> sa <*> sb
let lift3 f sa sb sc = f <$> sa <*> sb <*> sc
let lift4 f sa sb sc sd = f <$> sa <*> sb <*> sc <*> sd
let lift5 f sa sb sc sd se = f <$> sa <*> sb <*> sc <*> sd <*> se

let rec sequence = function
  | [] -> pure []
  | fx :: fxs -> lift2 (fun x xs -> x :: xs) fx (sequence fxs)

let rec join ssx =
  let propagateref = ref (fun _ -> ()) in
  let timed_value t =
    let (sx, ssx') = at ssx t in
    let (x, _) = at sx t in
    (x, join ssx')
  in
  register ssx (fun t -> !propagateref t);
  { timed_value; propagateref }

let return = pure
let bind mx f = join (map f mx)
let ( >>= ) = bind
let ( >> ) fx fy = fx >>= fun _ -> fy
let mapM f xs = sequence (List.map f xs)

let rec perform_state_post sx ~init ~f =
  register sx (fun t ->
             let (x, sx') = at sx t in
             let (init', post) = f init x in
             perform_state_post sx' ~init:init' ~f;
             post ()
           )

let perform_state sx ~init ~f =
  perform_state_post sx ~init ~f:(fun x y -> (f x y, (fun () -> ())))
let perform sx f = perform_state sx ~init:() ~f:(fun () x -> f x; ())

let react sx f = perform sx (function | None -> () | Some x -> f x)
let react_with sx sy f =
  let sxy = (fun x y -> match x with
                        | Some x -> Some (x, y)
                        | None   -> None
            ) <$> sx <*> sy
  in react sxy (fun (x, y) -> f x y)

let rec on sx ~init ~f =
  let propagateref = ref (fun _ -> ()) in
  let timed_value t =
    let (xopt, sx') = at sx t in
    let x' = match xopt with
      | Some x -> f init x
      | None -> init
    in
    (x', on sx' ~init:x' ~f)
  in
  register sx (fun t -> !propagateref t);
  { timed_value; propagateref }

let last sx = on sx ~f:(fun _ x -> x)
let toggle sx = on sx ~f:(fun x _ -> not x)
let count ?(init=0) sx = on sx ~init ~f:(fun x _ -> x + 1)

let rec upon ?init ev sx =
  let propagateref = ref (fun _ -> ()) in
  let timed_value t =
    let (opt, ev') = at ev t in
    let (x, sx') = at sx t in
    match opt with
    | Some _ -> (x, upon ~init:x ev' sx')
    | None ->
       let value = match init with
         | Some i -> i
         | None   -> x
       in (value, upon ~init:value ev' sx')
  in
  register sx (fun t -> !propagateref t);
  { timed_value; propagateref }

let rec fold sx ~init ~f =
  let propagateref = ref (fun _ -> ()) in
  let timed_value t =
    let (x, sx') = at sx t in
    let y = f init x in
    (y, fold sx' ~init:y ~f)
  in
  register sx (fun t -> !propagateref t);
  { timed_value; propagateref }

let event () =
  let module E = Event in
  let ev = E.create () in
  let tipref = ref ev in
  let propagatesref = ref [] in

  let trigger x =
    let (t, tip') = E.add !tipref (Sys.time ()) x in
    tipref := tip';
    (* Clean up propagate so that new signals can bind to it again *)
    let propagates = !propagatesref in
    propagatesref := [];
    List.iter (fun (_, f) -> f t) propagates
  in

  let rec mk_signal ev old_ident =
    let ident = Random.int32 Int32.max_int in
    let propagateref = ref (fun _ -> ()) in
    let timed_value t =
      let x   = E.at    ev t in
      let ev' = E.after ev t in
      (x, mk_signal ev' (Some ident))
    in
    (* Append this local propagate: after the first event, several signals
       could be created. We also need to remove the previous signal manually
       when the event has not been triggered. *)
    let propagates = match old_ident with
      | None    -> !propagatesref
      | Some id -> List.remove_assoc id !propagatesref
    in
    propagatesref := propagates @ [(ident, fun t -> !propagateref t)];
    { timed_value; propagateref }
  in
  let signal = mk_signal ev None in
  (signal, trigger)

let connected_event connect =
  let (signal, trigger) = event () in
  let disconnect = connect trigger in
  (signal, trigger, disconnect)
