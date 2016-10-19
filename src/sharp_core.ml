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
       | None -> r := Some (Cons (t, x, (create ()))); t
       | Some es' -> add es' t x

  let rec at es t = match es with
    | Cons (t', x, _) when t = t' -> Some x
    | Cons (t', _, _) when t > t' -> None
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

module type Behaviour_base_S = sig
  type ('a, 'b) t
  type 'a event = ('a option, 'a) t

  val time : (time, 'a) t

  val map : ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t
  val pure : 'a -> ('a, 'b) t
  val apply : ('a -> 'b, 'c) t -> ('a, 'd) t -> ('b, 'e) t
  val join : (('a, 'b) t, 'c) t -> ('a, 'd) t

  val contramap : ('a, 'b) t -> f:('c -> 'b) -> ('a, 'c) t

  val ( <$?> ) : ('a -> 'b) -> ('a option, 'c) t -> ('b option, 'c) t
  val ( <*?> ) :
    (('a -> 'b) option, 'c) t -> ('a option, 'd) t -> ('b option, 'e) t

  val event : unit -> ('a option, 'a) t
  val trigger : ('a, 'b) t -> 'b -> time option

  val combine : ('a, 'b) t -> ('c, 'd) t -> ('a, 'd) t

  val on : ('a option, 'b) t -> init:'c -> f:('c -> 'a -> 'c) -> ('c, 'b) t
  val last : ('a option, 'b) t -> init:'a -> ('a, 'b) t
  val toggle : ('a option, 'b) t -> init:bool -> (bool, 'b) t
  val count : ?init:int -> ('a option, 'b) t -> (int, 'b) t
  val upon : ?init:'a -> ('b option, 'c) t -> ('a, 'd) t -> ('a, 'c) t

  val fold : ('a -> 'b -> 'a) -> 'a -> ('b, 'c) t -> ('a, 'd) t
end

module Behaviour_base = struct
  type 'a behaviour = B of (time -> 'a * 'a behaviour)
  type 'a event_trigger = 'a -> time

  type ('a, 'b) t =
    { behaviour : 'a behaviour
    ; trigger   : 'b event_trigger option
    }

  type 'a event = ('a option, 'a) t

  let at { behaviour } t = let B f = behaviour in f t

  let time =
    let rec b = B (fun t -> (t, b)) in { behaviour = b; trigger = None }

  let rec mapb (B fa) ~f =
    let fb t =
      let (a, ba) = fa t in (f a, mapb ~f ba)
    in B fb

  let map { behaviour; trigger } ~f = { behaviour = mapb ~f behaviour; trigger }

  let pure a =
    let rec b = B (fun _ -> (a, b)) in { behaviour = b; trigger = None }

  let rec applyb (B bf) (B ba) =
    let g t =
      let (f, bf') = bf t in
      let (a, ba') = ba t in
      (f a, applyb bf' ba')
    in B g

  let apply { behaviour; trigger }
            { behaviour = behaviour'; trigger = trigger' } =
    { behaviour = applyb behaviour behaviour'; trigger = None }

  let rec joinb (B fba) =
    let f' t =
      let ({ behaviour = B fa }, bba) = fba t in
      let (a, _) = fa t in
      (a, joinb bba)
    in B f'

  let join { behaviour; trigger } =
    { behaviour = joinb behaviour; trigger = None }

  let contramap { behaviour; trigger } ~f =
    let trigger' = match trigger with
      | None   -> None
      | Some g -> Some (fun x -> g (f x))
    in { behaviour; trigger = trigger' }

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
    let tip = E.create () in
    let tipref = ref tip in
    let rec b tip =
      let f t =
        let x    = E.at tip t in
        let tip' = E.after tip t in
        tipref := tip'; (x, b tip')
      in B f
    in
    let add x = E.add !tipref (Sys.time ()) x in
    { behaviour = b tip; trigger = Some add }

  let trigger { trigger } x = match trigger with
    | None   -> None
    | Some f -> Some (f x)

  let combine { behaviour } { trigger } = { behaviour; trigger }

  let rec onb (B fa) ~init ~f =
    let f' t =
      match fa t with
      | (None,   b) -> (init, onb ~init ~f b)
      | (Some a, b) ->
         let init' = f init a in (init',  onb ~init:init' ~f b)
    in B f'

  let on { behaviour; trigger} ~init ~f =
    { behaviour = onb behaviour ~init ~f; trigger }

  let last   b ~init   = on b ~init ~f:(fun _ x -> x)
  let toggle b ~init   = on b ~init ~f:(fun i _ -> not i)
  let count  ?(init=0) = on ~init ~f:(fun i _ -> i + 1)

  let rec uponb ?init (B fa) (B fb) =
    let f t =
      let (a, ba) = fa t in
      let (b, bb) = fb t in
      match a with
      | Some _ -> (b, uponb ~init:b ba bb)
      | None   ->
         let value = match init with
           | Some x -> x
           | None   -> b
         in (value, uponb ~init:value ba bb)
    in B f

  let upon ?init { behaviour; trigger } { behaviour = behaviour' } =
    { behaviour = uponb ?init behaviour behaviour';  trigger }

  let rec foldb f init (B fa) =
    let f' t =
      let (a, ba) = fa t     in
      let init'   = f init a in
      (init', foldb f init' ba)
    in B f'

  let fold f init { behaviour } =
    { behaviour = foldb f init behaviour; trigger = None }
end

module Behaviour = struct
  module Base = Behaviour_base
  include Base
  include Monad.MakeNoInfix(Base)
  module Infix = Monad.Make(Base)
end

module Behavior = Behaviour

module type Network_base_S = sig
  type 'a t
  type ('a, 'b) secondary_t = 'a t

  val start : 'a t -> unit -> unit
  val add_funnel : ((time -> unit) -> unit -> unit) -> unit t
  val add_sink : (time -> unit) -> unit t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val join : 'a t t -> 'a t

  val perform_state_post : ('a, 'b) Behaviour.t -> init:'c
                           -> f:('c -> 'a -> 'c * (unit -> unit)) -> unit t
  val perform_state : ('a, 'b) Behaviour.t -> init:'c -> f:('c -> 'a -> 'c)
                      -> unit t
  val perform : ('a, 'b) Behaviour.t -> f:('a -> unit) -> unit t
  val react : ('a option, 'b) Behaviour.t -> ('c, 'd) Behaviour.t
              -> f:('a -> 'c -> unit) -> unit t

  val initially : (unit -> unit) -> unit t
  val finally : (unit -> unit) -> unit t
end

module Network_base = struct
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

  type ('a, 'b) secondary_t = 'a t

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

  let start { funnels; sinks; initialiser; finaliser } =
    let started = ref false in
    let signal t = flush_sinks t sinks in
    let proxy_signal t = if !started then signal t else () in
    let disconnect = connect (fun () -> finaliser ()) proxy_signal funnels in
    started := true; initialiser (); signal (Sys.time ()); disconnect

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

  let perform_state_post b ~init ~f =
    let bref     = ref b    in
    let stateref = ref init in
    let s t =
      let (x, b') = Behaviour.at !bref t in
      let state   = !stateref in
      bref := { Behaviour.behaviour = b'; trigger = None };
      let (state', post) = f state x in
      stateref := state'; post ()
    in add_sink s

  let perform_state b ~init ~f =
    perform_state_post b ~init ~f:(fun x y -> (f x y, fun () -> ()))

  let perform b ~f = perform_state ~init:() b ~f:(fun () x -> f x; ())

  let react e b ~f =
    perform (Behaviour.apply (Behaviour.map ~f:(fun x y -> (x, y)) b) e)
            (fun (bval, eval_opt) -> match eval_opt with
                                 | None -> ()
                                 | Some eval -> f eval bval)

  let initially f = { empty with initialiser = f }
  let finally   f = { empty with finaliser   = f }
end

module type Network_extra_S = sig
  type 'a t
  val event : (('a -> unit) -> (unit -> unit)) -> ('a option, 'a) Behaviour.t t
  val unbound_event : unit -> ('a option, 'a) Behaviour.t t
end

module Network_extra (M : sig
                        type 'a t
                        type ('a, 'b) secondary_t = 'a t
                        include Monad.NoInfix
                                with type ('a, 'b) t := ('a, 'b) secondary_t
                        val add_funnel :
                          ((time -> unit) -> unit -> unit) -> unit t
                      end) = struct
  let event (connect : ('a -> unit) -> unit -> unit) =
    let ({ Behaviour.trigger = trigger_opt } as ev) = Behaviour.event () in
    let flushref = ref (fun _ -> ()) in
    let connect' flush =
      flushref := flush;
      let add x =
        match Behaviour.trigger ev x with
        | None   -> ()
        | Some t -> flush t
      in
      let disconnect = connect add in
      fun () -> flushref := (fun _ -> ()); disconnect ()
    in
    let trigger_opt' = match trigger_opt with
      | None         -> None
      | Some trigger -> Some (fun x -> let t = trigger x in (!flushref) t; t)
    in
    M.bind (M.add_funnel connect')
           (fun _ -> M.pure { ev with Behaviour.trigger = trigger_opt' })

  let unbound_event () = event (fun _ _ -> ())
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
