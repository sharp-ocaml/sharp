open Sharp_category

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

module type Signal_base_S = sig
  type 'a timed_value   = TV of (time -> 'a * 'a timed_value)
  type 'a event_trigger = 'a -> time option

  type ('a, 'b) t =
    { trigger     : 'a event_trigger
    ; timed_value : 'b timed_value
    }

  type 'a event = ('a, 'a option) t

  val at : ('a, 'b) t  -> time -> 'b * ('a, 'b) t

  val time : (void, time) t

  val no_trigger : ('a, 'b) t -> (void, 'b) t

  val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
  val pure : 'a -> (void, 'a) t
  val apply : ('a, 'b -> 'c) t -> ('d, 'b) t -> (void, 'c) t
  val join : ('a, ('b, 'c) t) t -> (void, 'c) t

  val contramap : ('a, 'b) t -> f:('c -> 'a) -> ('c, 'b) t
  val contramap_opt : ('a, 'b) t -> f:('c -> 'a option) -> ('c, 'b) t

  val ( <$?> ) : ('a -> 'b) -> ('c, 'a option) t -> ('c, 'b option) t
  val ( <*?> ) :
    ('a, ('b -> 'c) option) t -> ('d, 'b option) t -> (void, 'c option) t

  val event : unit -> 'a event
  val trigger : ('a, 'b) t -> 'a -> unit
  val trigger' : ('a, 'b) t -> 'a -> time option

  val combine : ('a, 'b) t -> ('c, 'd) t -> ('a, 'd) t

  val on : ('a, 'b option) t -> init:'c -> f:('c -> 'b -> 'c) -> ('a, 'c) t
  val last : ('a, 'b option) t -> init:'b -> ('a, 'b) t
  val toggle : ('a, 'b option) t -> init:bool -> ('a, bool) t
  val count : ?init:int -> ('a, 'b option) t -> ('a, int) t
  val upon : ?init:'a -> ('b, 'c option) t -> ('d, 'a) t -> (void, 'a) t

  val fold : ('c, 'b) t -> f:('a -> 'b -> 'a) -> init:'a -> ('c, 'a) t
end

module Signal_base : Signal_base_S = struct
  type 'a timed_value   = TV of (time -> 'a * 'a timed_value)
  type 'a event_trigger = 'a -> time option

  type ('a, 'b) t =
    { trigger     : 'a event_trigger
    ; timed_value : 'b timed_value
    }

  type 'a event = ('a, 'a option) t

  let at { timed_value = TV f; trigger } t =
    let (x, timed_value) = f t in
    (x, { timed_value; trigger })

  let void_trigger _ = None

  let time =
    let rec timed_value = TV (fun t -> (t, timed_value)) in
    { timed_value; trigger = void_trigger }

  let no_trigger s = { s with trigger = void_trigger }

  let rec maptv (TV fa) ~f =
    let fb t =
      let (a, ba) = fa t in (f a, maptv ~f ba)
    in TV fb

  let map { timed_value; trigger } ~f =
    { timed_value = maptv ~f timed_value; trigger }

  let pure a =
    let rec timed_value = TV (fun _ -> (a, timed_value)) in
    { timed_value; trigger = void_trigger }

  let rec applytv (TV tvf) (TV tva) =
    let g t =
      let (f, bf') = tvf t in
      let (a, ba') = tva t in
      (f a, applytv bf' ba')
    in TV g

  let apply { timed_value } { timed_value = timed_value' } =
    { timed_value = applytv timed_value timed_value'
    ; trigger     = void_trigger
    }

  let rec jointv (TV ftva) =
    let f' t =
      let ({ timed_value = TV fa }, tvtva) = ftva t in
      let (a, _) = fa t in
      (a, jointv tvtva)
    in TV f'

  let join { timed_value } =
    { timed_value = jointv timed_value; trigger = void_trigger }

  let contramap { timed_value; trigger } ~f =
    { timed_value; trigger = (fun x -> trigger (f x)) }

  let contramap_opt { timed_value; trigger } ~f =
    let trigger' x = match f x with
      | None -> None
      | Some y -> trigger y
    in { timed_value; trigger = trigger' }

  let map_opt f = function
    | None   -> None
    | Some x -> Some (f x)

  let app_opt fopt aopt = match fopt, aopt with
    | Some f, Some a -> Some (f a)
    | _ -> None

  let ( <$?> ) f b = map ~f:(map_opt f) b
  let rec ( <*?> ) bf ba = apply (map ~f:app_opt bf) ba

  let event () =
    let module E = Event in
    let rec tv ev =
      let f t =
        let x   = E.at    ev t in
        let ev' = E.after ev t in
        (x, tv ev')
      in TV f
    in
    let ev = E.create () in
    let tipref = ref ev in
    let add x =
      let (t, tip') = E.add !tipref (Sys.time ()) x in
      tipref := tip'; Some t
    in
    { timed_value = tv ev; trigger = add }

  let trigger { trigger } x = let _ = trigger x in ()
  let trigger' { trigger } = trigger

  let combine { trigger } { timed_value } = { timed_value; trigger }

  let rec ontv (TV fa) ~init ~f =
    let f' t =
      match fa t with
      | (None,   tv) -> (init, ontv ~init ~f tv)
      | (Some a, tv) ->
         let init' = f init a in (init', ontv ~init:init' ~f tv)
    in TV f'

  let on { timed_value; trigger } ~init ~f =
    { timed_value = ontv timed_value ~init ~f; trigger }

  let last   tv ~init   = on tv ~init ~f:(fun _ x -> x)
  let toggle tv ~init   = on tv ~init ~f:(fun i _ -> not i)
  let count  ?(init=0)  = on ~init ~f:(fun i _ -> i + 1)

  let rec uponb ?init (TV fa) (TV fb) =
    let f t =
      let (a, tva) = fa t in
      let (b, tvb) = fb t in
      match a with
      | Some _ -> (b, uponb ~init:b tva tvb)
      | None   ->
         let value = match init with
           | Some x -> x
           | None   -> b
         in (value, uponb ~init:value tva tvb)
    in TV f

  let upon ?init { timed_value } { timed_value = timed_value' } =
    { timed_value = uponb ?init timed_value timed_value'
    ; trigger = void_trigger
    }

  let rec foldtv f init (TV fa) =
    let f' t =
      let (a, tva) = fa t     in
      let init'    = f init a in
      (init', foldtv f init' tva)
    in TV f'

  let fold { timed_value; trigger } ~f ~init =
    { timed_value = foldtv f init timed_value; trigger }
end

module Signal = struct
  module Base = Signal_base
  include Base
  include Monad.MakeNoInfix(Base)
  module Infix = Monad.Make(Base)
end

module type Network_base_S = sig
  type 'a t
  type ('x, 'a) secondary_t = 'a t
  type manager

  val noop_manager : manager

  val start : 'a t -> manager
  val flush : manager -> time -> unit
  val stop  : manager -> unit

  val add_funnel : ((time -> unit) -> unit -> unit) -> unit t
  val add_sink : (time -> unit) -> unit t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val join : 'a t t -> 'a t

  val perform_state_post : ?finally:('a -> unit) -> ('b, 'c) Signal.t
                           -> init:'a -> f:('a -> 'c -> 'a * (unit -> unit))
                           -> unit t
  val perform_state : ?finally:('a -> unit) -> ('b, 'c) Signal.t -> init:'a
                      -> f:('a -> 'c -> 'a) -> unit t
  val perform : ('a, 'b) Signal.t -> f:('b -> unit) -> unit t
  val react : ('a, 'b option) Signal.t -> ('c, 'd) Signal.t
              -> f:('b -> 'd -> unit) -> unit t
  val react_ : ('a, 'b option) Signal.t -> f:('b -> unit) -> unit t

  val initially : (unit -> unit) -> unit t
  val finally : (unit -> unit) -> unit t
end

module Network_base : Network_base_S = struct
  (* Receive a function to signal a new event and return a function to
   * disconnect the funnel *)
  type funnel = F of ((time -> unit) -> (unit -> unit))

  type sink = S of (time -> unit)

  type 'a t =
    { value       : 'a
    ; funnels     : funnel list
    ; sinks       : sink list
    ; initialiser : unit -> unit
    ; finaliser   : unit -> unit
    }

  type ('x, 'a) secondary_t = 'a t

  type manager =
    { flush : time -> unit
    ; stop  : unit -> unit
    }

  let noop_manager = { flush = (fun _ -> ()); stop = (fun _ -> ()) }
  (* helper for start *)
  let rec connect acc signal = function
    | [] -> acc
    | F f :: fs' ->
       let disconnect = f signal in
       let acc' () = acc (disconnect ()) in
       connect acc' signal fs'

  let rec flush_sinks t = function
    | [] -> ()
    | S s :: sinks' -> s t; flush_sinks t sinks'

  let rec sequentially (f : 'a -> unit) =
    let stateref = ref None in
    let rec consume () = match !stateref with
      | Some [] -> stateref := None
      | Some (x :: xs) -> stateref := Some xs; f x; consume ()
      | None -> assert false
    in
    fun x ->
    match !stateref with
    | None -> stateref := Some [x]; consume ()
    | Some xs -> stateref := Some (xs @ [x])

  let start { funnels; sinks; initialiser; finaliser } =
    let started = ref false in
    let signal = sequentially (fun t -> flush_sinks t sinks) in
    let proxy_signal t = if !started then signal t else () in
    let disconnect = connect (fun () -> finaliser ()) proxy_signal funnels in
    started := true; initialiser (); signal (Sys.time ());
    { flush = signal; stop = disconnect }

  let flush { flush } = flush
  let stop  { stop  } = stop ()

  let empty =
    { value       = ()
    ; funnels     = []
    ; sinks       = []
    ; initialiser = (fun () -> ())
    ; finaliser   = (fun () -> ())
    }

  let add_funnel f = { empty with funnels = [F f] }
  let add_sink   s = { empty with sinks   = [S s] }

  let map ({ value } as network) ~f = { network with value = f value }

  let pure x = { empty with value = x }

  let apply { value = f; funnels; sinks; initialiser; finaliser }
            { value = x; funnels = funnels'; sinks = sinks';
              initialiser = initialiser'; finaliser = finaliser' } =
    { value       = f x
    ; funnels     = funnels @ funnels'
    ; sinks       = sinks @ sinks'
    ; initialiser = (fun () -> initialiser' (initialiser ()))
    ; finaliser   = (fun () -> finaliser' (finaliser ()))
    }

  let join { value; funnels; sinks; initialiser; finaliser } =
    let { value = value'; funnels = funnels'; sinks = sinks';
          initialiser = initialiser'; finaliser = finaliser' } = value
    in
    { value       = value'
    ; funnels     = funnels @ funnels'
    ; sinks       = sinks @ sinks'
    ; initialiser = (fun () -> initialiser' (initialiser ()))
    ; finaliser   = (fun () -> finaliser' (finaliser ()))
    }

  let initially f = { empty with initialiser = f }
  let finally   f = { empty with finaliser   = f }

  let finally' = finally
  let perform_state_post ?(finally=fun _ -> ()) s ~init ~f =
    let sref     = ref s    in
    let stateref = ref init in
    let sink t =
      let (x, s') = Signal.at !sref t in
      let state   = !stateref in
      sref := s';
      let (state', post) = f state x in
      stateref := state'; post ()
    in
    let x = finally' (fun () -> finally (!stateref)) in
    let y = add_sink sink in
    join (map x ~f:(fun () -> y))

  let perform_state ?finally b ~init ~f =
    perform_state_post ?finally b ~init ~f:(fun x y -> (f x y, fun () -> ()))

  let perform b ~f = perform_state ~init:() b ~f:(fun () x -> f x; ())

  let react e b ~f =
    perform (Signal.apply (Signal.map ~f:(fun x y -> (x, y)) b) e)
            (fun (bval, eval_opt) -> match eval_opt with
                                     | None -> ()
                                     | Some eval -> f eval bval)

  let react_ e ~f =
    perform e (fun eval_opt -> match eval_opt with
                               | None -> ()
                               | Some eval -> f eval)
end

module type Network_extra_S = sig
  type 'a t
  val event : ?connect:(('a -> unit) -> (unit -> unit)) -> unit
              -> 'a Signal.event t
end

module Network_extra (M : sig
                        type 'a t
                        type ('x, 'a) secondary_t = 'a t
                        include Monad.NoInfix
                                with type ('a, 'b) t := ('a, 'b) secondary_t
                        val add_funnel :
                          ((time -> unit) -> unit -> unit) -> unit t
                      end) = struct
  let event ?(connect=fun _ _ -> ()) () =
    let ({ Signal.trigger = trigger } as ev) = Signal.event () in
    let flushref = ref (fun _ -> ()) in
    let connect' flush =
      flushref := flush;
      let add x =
        match Signal.trigger' ev x with
        | None   -> ()
        | Some t -> flush t
      in
      let disconnect = connect add in
      fun () -> flushref := (fun _ -> ()); disconnect ()
    in
    let trigger' x = match trigger x with
      | None   -> None
      | Some t -> (!flushref) t; Some t
    in
    M.bind (M.add_funnel connect')
           (fun _ -> M.pure { ev with Signal.trigger = trigger' })
end

module Network = struct
  module Base = Network_base
  include Base

  module BaseWithSecondary = struct
    type ('a, 'b) t = ('a, 'b) Base.secondary_t
    let map   = Base.map
    let pure  = Base.pure
    let apply = Base.apply
    let join  = Base.join
  end
  include Monad.MakeNoInfix(BaseWithSecondary)

  include Network_extra(struct
                         include Base
                         include Monad.MakeNoInfix(BaseWithSecondary)
                       end)

  module Infix = Monad.Make(BaseWithSecondary)
end
