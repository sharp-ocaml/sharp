open Sharp_core

open Behaviour
open Network

open Dom
open Dom_html

type attributes = (string * string) list
type restart_strategy =
  | Never | Always | OnChange | OnDeepChange | OnIdentifierChange of string

module type Extra = sig
  type 'a t
end

module Generic (E : Extra) = struct
  type t =
    | Node of string * string option * attributes * t list * restart_strategy
              * element Js.t E.t
    | Text of string * restart_strategy * text Js.t E.t

  let append_child child parent = match parent with
    | Node (name, id_opt, attrs, children, strategy, extra) ->
       Node (name, id_opt, attrs, children @ [child], strategy, extra)
    | Text _ as t -> t

  let prepend_child child parent = match parent with
    | Node (name, id_opt, attrs, children, strategy, extra) ->
       Node (name, id_opt, attrs, child :: children, strategy, extra)
    | Text _ as t -> t

  let set_attribute attr_name attr_value = function
    | Node (name, id_opt, attrs, children, strategy, extra) ->
       let attrs' = (attr_name, attr_value) :: List.remove_assoc name attrs in
       Node (name, id_opt, attrs', children, strategy, extra)
    | Text _ as t -> t

  let clear_attribute name = function
    | Node (name, id_opt, attrs, children, strategy, extra) ->
       let attrs' = List.remove_assoc name attrs in
       Node (name, id_opt, attrs', children, strategy, extra)
    | Text _ as t -> t

  let ( |- ) parent child = parent |> append_child child
  let rec ( |+ ) parent = function
    | [] -> parent
    | child :: children -> append_child child parent |+ children
  let ( |* ) node (name, value) = node |> set_attribute name value
end

module Raw : sig
  type 'a t = 'a -> Network.manager
  val empty : 'a t
end = struct
  type 'a t = 'a -> Network.manager
  let empty _ = Network.noop_manager
end

module LinkedExtra = struct
  type 'a t = 'a * (time -> unit) * (unit -> unit) (* element, flush, stop *)
end

module Linked = Generic(LinkedExtra)
include Generic(Raw)

let node ?network ?(strategy=OnDeepChange) ?id_opt name children =
  let f = match network with
    | None     -> Raw.empty
    | Some net -> fun node -> Network.start (net node)
  in
  Node (name, id_opt, [], children, strategy, f)
let element ?network ?strategy ?id_opt name =
  node ?network ?strategy ?id_opt name []
let tag = element
let text ?network ?(strategy=OnDeepChange) content =
  let f = match network with
    | None     -> Raw.empty
    | Some net -> fun node -> Network.start (net node)
  in Text (content, strategy, f)

let make_callback_functions start node =
  let linked   = ref true in
  let flushref = ref (fun _ -> ()) in
  let stopref  = ref (fun () -> linked := false) in

  let callback _ = if !linked
                   then let manager = start node in
                        flushref := Network.flush manager;
                        stopref := (fun () ->
                          flushref := (fun _ -> ());
                          Network.stop manager
                        )
                   else ()

  in ((fun t -> (!flushref) t), (fun () -> (!stopref) ()), callback)

let create_node name attrs =
  let node = document##createElement (Js.string name) in
  let _    =
    List.map (fun (n,v) -> node##setAttribute (Js.string n) (Js.string v))
             attrs
  in node

let insert_in_real_dom ?current parent node =
  match current with
    | None   -> appendChild  parent node
    | Some n -> replaceChild parent node n

let rec link ?current parent vdom = match vdom with
  | Node (name, id_opt, attrs, children, strategy, start) ->
     let node = create_node name attrs in
     let _ = insert_in_real_dom ?current parent node in

     let pairs         = List.map (link node) children in
     let children'     = List.map (fun (n,_) -> n) pairs in
     let subcallback t = List.iter (fun (_,c) -> c t) pairs in
     (* children created before initialisation *)
     let (flush, stop, callback) = make_callback_functions start node in
     let linked_node =
       Linked.Node (name, id_opt, attrs, children', strategy,
                    (node, flush, stop))
     in
     (linked_node, fun t -> subcallback t; callback t)

  | Text (str, strategy, start) ->
     let node = document##createTextNode (Js.string str) in
     let _ = insert_in_real_dom ?current parent node in
     let (flush, stop, callback) = make_callback_functions start node in
     (Linked.Text (str, strategy, (node, flush, stop)), callback)

let rec unlink vdom = match vdom with
  | Linked.Node (_, _, _, children, _, (node, _, stop)) ->
     let substops  = List.map unlink children in
     let substop t = List.iter (fun f -> f t) substops in
     Js.Opt.iter (node##.parentNode)
                 (fun parent -> removeChild parent node);
     fun t -> substop t; stop ()
  | Linked.Text (_, _, (_, _, stop)) ->
     fun _ -> stop () (* removeChild fails on texts *)

let rec update_attributes node old_attrs new_attrs =
  let set name value = node##setAttribute (Js.string name) (Js.string value) in
  let remove name    = node##removeAttribute (Js.string name) in

  let rec go acc old_attrs new_attrs = match old_attrs, new_attrs with
    | [], [] -> acc
    | [], attrs -> let _ = List.iter (fun (n,v) -> set n v)  attrs in true
    | attrs, [] -> let _ = List.iter (fun (n,_) -> remove n) attrs in true
    | (name, value) :: old_attrs', _ ->
       match List.partition (fun (n,_) -> n = name) new_attrs with
       | [], _ -> remove name; go true old_attrs' new_attrs
       | (_, value') :: _, new_attrs' when value = value' ->
          go acc old_attrs' new_attrs'
       | (_, value') :: _, new_attrs' ->
          set name value'; go true old_attrs' new_attrs'

  in go false old_attrs new_attrs

let rec fold_left2_opt ~f acc xs ys = match xs, ys with
  | [], [] -> acc
  | x :: xs', [] ->
     let acc' = f acc (Some x) None in fold_left2_opt ~f acc' xs' []
  | [], y :: ys' ->
     let acc' = f acc None (Some y) in fold_left2_opt ~f acc' [] ys'
  | x :: xs', y :: ys' ->
     let acc' = f acc (Some x) (Some y) in fold_left2_opt ~f acc' xs' ys'

let restart_needed strategy strategy' situation =
  match strategy, strategy', situation with
  | _, Always, _                                   -> true
  | _, Never,  _                                   -> false
  | _, _, `NothingChanged                          -> false
  | _, OnChange, `CurrentNodeChanged               -> true
  | _, OnChange, `OnlyChildrenChanged              -> false
  | _, OnDeepChange, `CurrentNodeChanged           -> true
  | _, OnDeepChange, `OnlyChildrenChanged          -> true
  | OnIdentifierChange i, OnIdentifierChange i', _ -> i != i'
  | _, OnIdentifierChange _, _                     -> true

let rec diff_and_patch_opt parent vdom_opt vdom_opt' =
  match vdom_opt, vdom_opt' with
  | None, Some vdom' ->
     let (vdom'', callback) = link parent vdom' in
     (Some vdom'', callback, true)
  | Some vdom, None ->
     let callback = unlink vdom in (None, callback, true)
  | None, None -> assert false

  | Some (Linked.Node (name, id_opt, attrs, children, strategy,
                       (node, flush, stop))),
    Some (Node (name', id_opt', attrs', children', strategy', start))
       when name = name' && id_opt = id_opt' ->
     let node_changed = update_attributes node attrs attrs' in
     let (children'', subcallbacks, children_changed) =
       fold_left2_opt ([], [], false) children children'
                      ~f:(fun (children'', callbacks, changed) child child' ->
                        let (child_opt, callback, changed') =
                          diff_and_patch_opt node child child'
                        in
                        let callbacks' = callbacks @ [callback] in
                        let changed''  = changed || changed' in
                        match child_opt with
                        | None -> (children'', callbacks', true)
                        | Some child'' ->
                           (children'' @ [child''], callbacks', changed'')
                      )
     in
     let subcallback t = List.iter (fun k -> k t) subcallbacks in

     let changed = node_changed || children_changed in
     let situation = match node_changed, children_changed with
       | true,  _     -> `CurrentNodeChanged
       | false, true  -> `OnlyChildrenChanged
       | false, false -> `NothingChanged
     in

     let (flush', stop', full_callback) =
       if restart_needed strategy strategy' situation
       then let start' node = stop (); start node in
            let (flush, stop, callback) = make_callback_functions start' node in
            (flush, stop, fun t -> subcallback t; callback t)
       else (flush, stop, fun t -> subcallback t; flush t)
     in
     let linked_node = Linked.Node (name, id_opt, attrs', children'', strategy',
                                    (node, flush', stop')) in

     (Some linked_node, full_callback, changed)

  | Some (Linked.Text (str, strategy, (node, flush, stop))),
    Some (Text (str', strategy', start)) when str != str' ->
     let _ = node##.data := Js.string str' in

     let (flush', stop', callback) =
       if restart_needed strategy strategy' `CurrentNodeChanged
       then let start' node = stop (); start node in
            make_callback_functions start' node
       else (flush, stop, fun t -> flush t)
     in

     let linked_node = Linked.Text (str', strategy', (node, flush', stop')) in
     (Some linked_node, callback, false) (* text change = no change *)

  | Some (Linked.Text (str, strategy, (node, flush, stop))),
    Some (Text (_, strategy', start)) ->
     let (flush', stop', callback) =
       if restart_needed strategy strategy' `NothingChanged
       then let start' node = stop (); start node in
            make_callback_functions start' node
       else (flush, stop, fun t -> flush t)
     in
     let linked_node = Linked.Text (str, strategy, (node, flush', stop')) in
     (Some linked_node, callback, false)

  | Some vdom, Some vdom' ->
     let callback = unlink vdom in
     let (vdom'', callback') = link parent vdom' in
     let callback'' t = callback t; callback' t in
     (Some vdom'', callback'', true)

let diff_and_patch parent vdom vdom' =
  let (vdom_opt, callback, _) =
    diff_and_patch_opt parent (Some vdom) (Some vdom')
  in match vdom_opt with
     | Some vdom'' -> (vdom'', callback)
     | None -> callback 0.; assert false

let vdom parent b f =
  let open Behaviour.Infix in
  let dat = (fun x y -> (x, y)) <$> Behaviour.time <*> b in

  let open Network.Infix in
  let rec g vdom_opt (t, x) =
    let vdom' = f x in
    let (linked, callback) = match vdom_opt with
      | None      -> link parent vdom'
      | Some vdom -> diff_and_patch parent vdom vdom'
    in (Some linked, fun () -> callback t)
  in

  perform_state_post ~init:None ~f:g
                     ~finally:(fun vdom_opt ->
                       match vdom_opt with
                       | None -> ()
                       | Some vdom -> unlink vdom ()
                     )
                     dat

let vdom_ parent f = vdom parent (Behaviour.pure ()) f

(* Helpers for specific elements *)
module Element = struct
  let a ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.a node)
                          (fun () -> assert false) network
            ) "a"

  let area ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.area node)
                          (fun () -> assert false) network
            ) "area"

  let article ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network "article"

  let base ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.base node)
                          (fun () -> assert false) network
            ) "base"

  let blockquote ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.blockquote node)
                          (fun () -> assert false) network
            ) "blockquote"

  let body ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.body node)
                          (fun () -> assert false) network
            ) "body"

  let br ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.br node)
                          (fun () -> assert false) network
            ) "br"

  let button ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.button node)
                          (fun () -> assert false) network
            ) "button"

  let canvas ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.canvas node)
                          (fun () -> assert false) network
            ) "canvas"

  let caption ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.caption node)
                          (fun () -> assert false) network
            ) "caption"

  let col ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.col node)
                          (fun () -> assert false) network
            ) "col"

  let colgroup ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.colgroup node)
                          (fun () -> assert false) network
            ) "colgroup"

  let del ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.del node)
                          (fun () -> assert false) network
            ) "del"

  let div ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.div node)
                          (fun () -> assert false) network
            ) "div"

  let dl ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.dl node)
                          (fun () -> assert false) network
            ) "dl"

  let fieldset ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.fieldset node)
                          (fun () -> assert false) network
            ) "fieldset"

  let embed ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.embed node)
                          (fun () -> assert false) network
            ) "embed"

  let form ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.form node)
                          (fun () -> assert false) network
            ) "form"

  let frameset ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.frameset node)
                          (fun () -> assert false) network
            ) "frameset"

  let frame ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.frame node)
                          (fun () -> assert false) network
            ) "frame"

  let h1 ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h1 node)
                          (fun () -> assert false) network
            ) "h1"

  let h2 ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h2 node)
                          (fun () -> assert false) network
            ) "h2"

  let h3 ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h3 node)
                          (fun () -> assert false) network
            ) "h3"

  let h4 ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h4 node)
                          (fun () -> assert false) network
            ) "h4"

  let h5 ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h5 node)
                          (fun () -> assert false) network
            ) "h5"

  let h6 ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h6 node)
                          (fun () -> assert false) network
            ) "h6"

  let head ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.head node)
                          (fun () -> assert false) network
            ) "head"

  let header ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network "header"

  let hr ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.hr node)
                          (fun () -> assert false) network
            ) "hr"

  let html ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.html node)
                          (fun () -> assert false) network
            ) "html"

  let iframe ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.iframe node)
                          (fun () -> assert false) network
            ) "iframe"

  let img ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.img node)
                          (fun () -> assert false) network
            ) "img"

  let input ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.input node)
                          (fun () -> assert false) network
            ) "input"

  let ins ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ins node)
                          (fun () -> assert false) network
            ) "ins"

  let label ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.label node)
                          (fun () -> assert false) network
            ) "label"

  let legend ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.legend node)
                          (fun () -> assert false) network
            ) "legend"

  let li ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.li node)
                          (fun () -> assert false) network
            ) "li"

  let link ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.link node)
                          (fun () -> assert false) network
            ) "link"

  let map ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.map node)
                          (fun () -> assert false) network
            ) "map"

  let meta ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.meta node)
                          (fun () -> assert false) network
            ) "meta"

  let nav ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network "nav"

  let _object ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo._object node)
                          (fun () -> assert false) network
            ) "object"

  let ol ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ol node)
                          (fun () -> assert false) network
            ) "ol"

  let optgroup ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.optgroup node)
                          (fun () -> assert false) network
            ) "optgroup"

  let option ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.option node)
                          (fun () -> assert false) network
            ) "option"

  let p ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.p node)
                          (fun () -> assert false) network
            ) "p"

  let param ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.param node)
                          (fun () -> assert false) network
            ) "param"

  let pre ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.pre node)
                          (fun () -> assert false) network
            ) "pre"

  let q ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.q node)
                          (fun () -> assert false) network
            ) "q"

  let script ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.script node)
                          (fun () -> assert false) network
            ) "script"

  let section ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network "section"

  let select ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.select node)
                          (fun () -> assert false) network
            ) "select"

  let style ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.style node)
                          (fun () -> assert false) network
            ) "style"

  let table ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.table node)
                          (fun () -> assert false) network
            ) "table"

  let tbody ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tbody node)
                          (fun () -> assert false) network
            ) "tbody"

  let td ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.td node)
                          (fun () -> assert false) network
            ) "td"

  let textarea ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.textarea node)
                          (fun () -> assert false) network
            ) "textarea"

  let tfoot ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tfoot node)
                          (fun () -> assert false) network
            ) "tfoot"

  let th ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.th node)
                          (fun () -> assert false) network
            ) "th"

  let thead ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.thead node)
                          (fun () -> assert false) network
            ) "thead"

  let title ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.title node)
                          (fun () -> assert false) network
            ) "title"

  let tr ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tr node)
                          (fun () -> assert false) network
            ) "tr"

  let ul ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ul node)
                          (fun () -> assert false) network
            ) "ul"

  let audio ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.audio node)
                          (fun () -> assert false) network
            ) "audio"

  let video ?strategy ?id_opt network =
    element ?strategy ?id_opt ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.video node)
                          (fun () -> assert false) network
            ) "video"
end

module E = Element
