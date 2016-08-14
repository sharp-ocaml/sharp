type time = float

module Event = struct
  type 'a t =
    | Cons of time * 'a * 'a t
    | Stop of 'a t option ref

  let create () = Stop (ref None)

  (* TODO: If an element with the same time already exists, bump the time *)
  let rec add es t x = match es with
    | Cons (_, _, es') -> add es' t x
    | Stop r ->
       match !r with
       | None -> r := Some (Cons (t, x, (create ())))
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

module Behaviour = struct
  type 'a t = B of (time -> 'a * 'a t)

  let at (B f) t = f t

  let time =
    let rec b = B (fun t -> (t, b)) in b

  let lift0 x =
    let rec b = B (fun _ -> (x, b)) in b

  let rec lift1 ~f (B fa) =
    let fb t =
      let (a, ba) = fa t in (f a, lift1 ~f ba)
    in B fb

  let rec lift2 ~f (B fa) (B fb) =
    let fc t =
      let (a, ba) = fa t in
      let (b, bb) = fb t in
      (f a b, lift2 ~f ba bb)
    in B fc

  let rec lift3 ~f (B fa) (B fb) (B fc) =
    let fd t =
      let (a, ba) = fa t in
      let (b, bb) = fb t in
      let (c, bc) = fc t in
      (f a b c, lift3 ~f ba bb bc)
    in B fd

  let const = lift0
  let map = lift1

  let ( <$> ) f b = map f b
  let rec ( <*> ) (B bf) (B ba) =
    let g t =
      let (f, bf') = bf t in
      let (a, ba') = ba t in
      (f a, bf' <*> ba')
    in B g

  let ( <* ) ba bb = (fun a _ -> a) <$> ba <*> bb
  let ( *> ) ba bb = (fun _ b -> b) <$> ba <*> bb

  let event () =
    let module E = Event in
    let tip = ref (E.create ()) in
    let rec b =
      let f t =
        let e = !tip in
        let x = E.at e t in
        tip := E.after e t; (x, b)
      in B f
    in
    let add t x = E.add !tip t x in
    (b, add)

  let rec on ~init ~f (B fa) =
    let f' t =
      match fa t with
      | (None,   b) -> (init, on ~init ~f b)
      | (Some a, b) ->
         let init' = f init a in (init',  on ~init:init' ~f b)
    in B f'

  let last   ~init     = on ~init ~f:(fun _ x -> x)
  let toggle ~init     = on ~init ~f:(fun i _ -> not i)
  let count  ?(init=0) = on ~init ~f:(fun i _ -> i + 1)
end

module Behavior = Behaviour

module Network = struct
  (* Receive a function to signal a new event and return a function to
   * disconnect the funnel *)
  type funnel = F of ((time -> unit) -> (unit -> unit))

  type sink = S of (time -> unit)

  type 'a t =
    { value : 'a;
      funnels : funnel list;
      sinks : sink list;
    }

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

  let start { funnels; sinks } =
    let started = ref false in
    let signal t = flush_sinks t sinks in
    let proxy_signal t = if !started then signal t else () in
    let disconnect = connect (fun () -> ()) proxy_signal funnels in
    started := true; signal (Sys.time ()); disconnect

  let add_funnel f = { value = (); funnels = [F f]; sinks = []    }
  let add_sink   s = { value = (); funnels = [];    sinks = [S s] }

  let map ~f { value; funnels; sinks } = { value = f value; funnels; sinks }

  let return x = { value = x; funnels = []; sinks = [] }

  let bind { value; funnels; sinks } f =
    let { value = value'; funnels = funnels'; sinks = sinks' } = f value in
    { value = value'; funnels = funnels @ funnels'; sinks = sinks @ sinks' }

  let (>>=) = bind
  let (>>) mx my = mx >>= fun _ -> my

  let react b ~f =
    let bref = ref b in
    let s t =
      let (x, b') = Behaviour.at !bref t in
      bref := b'; f x
    in add_sink s
end
