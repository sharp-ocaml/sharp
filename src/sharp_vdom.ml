open Sharp_core

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
    | Node (name, id, attrs, children, strategy, extra) ->
       Node (name, id, attrs, children @ [child], strategy, extra)
    | Text _ as t -> t

  let prepend_child child parent = match parent with
    | Node (name, id, attrs, children, strategy, extra) ->
       Node (name, id, attrs, child :: children, strategy, extra)
    | Text _ as t -> t

  let set_attribute attr_name attr_value = function
    | Node (name, id, attrs, children, strategy, extra) ->
       let attrs' = (attr_name, attr_value) :: List.remove_assoc name attrs in
       Node (name, id, attrs', children, strategy, extra)
    | Text _ as t -> t

  let clear_attribute name = function
    | Node (name, id, attrs, children, strategy, extra) ->
       let attrs' = List.remove_assoc name attrs in
       Node (name, id, attrs', children, strategy, extra)
    | Text _ as t -> t

  let ( |- ) parent child = parent |> append_child child
  let rec ( |+ ) parent = function
    | [] -> parent
    | child :: children -> append_child child parent |+ children
  let ( |* ) node (name, value) = node |> set_attribute name value
end

module Raw : sig
  type 'a t = 'a -> (unit -> unit)
  val empty : 'a t
end = struct
  type 'a t = 'a -> (unit -> unit)
  let empty _ _ = ()
end

module LinkedExtra = struct
  type 'a t = 'a * (unit -> unit) (* element, stop *)
end

module Linked = Generic(LinkedExtra)
include Generic(Raw)

let node ?network ?(strategy=OnDeepChange) ?id name children =
  let f = match network with
    | None     -> Raw.empty
    | Some net -> net
  in
  Node (name, id, [], children, strategy, f)

let element ?network ?strategy ?id name =
  node ?network ?strategy ?id name []

let tag = element

let text ?network ?(strategy=OnDeepChange) content =
  let f = match network with
    | None     -> Raw.empty
    | Some net -> net
  in Text (content, strategy, f)

let make_callback_functions start node =
  let linked  = ref true in
  let stopref = ref (fun () -> linked := false) in

  let callback _ = if !linked
                   then let stop = start node in stopref := stop
                   else ()

  in ((fun () -> (!stopref) ()), callback)

let create_node name attrs =
  let node = document##createElement (Js.string name) in
  let _    =
    List.map (fun (n,v) -> node##setAttribute (Js.string n) (Js.string v))
             attrs
  in node

let insert_in_real_dom ?current parent node = match current with
  | None   -> appendChild  parent node
  | Some n -> replaceChild parent node n

let rec link ?current parent vdom = match vdom with
  | Node (name, id, attrs, children, strategy, start) ->
     let node = create_node name attrs in
     let _ = insert_in_real_dom ?current parent node in

     let pairs         = List.map (link node) children in
     let children'     = List.map (fun (n,_) -> n) pairs in
     let subcallback t = List.iter (fun (_,c) -> c t) pairs in
     (* children created before initialisation *)
     let (stop, callback) = make_callback_functions start node in
     let linked_node =
       Linked.Node (name, id, attrs, children', strategy, (node, stop))
     in
     (linked_node, fun t -> subcallback t; callback t)

  | Text (str, strategy, start) ->
     let node = document##createTextNode (Js.string str) in
     let _ = insert_in_real_dom ?current parent node in
     let (stop, callback) = make_callback_functions start node in
     (Linked.Text (str, strategy, (node, stop)), callback)

let rec unlink ?(remove=true) vdom = match vdom with
  | Linked.Node (_, _, _, children, _, (node, stop)) ->
     let substops  = List.map (unlink ~remove) children in
     let substop t = List.iter (fun f -> f t) substops in
     if remove
     then Js.Opt.iter (node##.parentNode)
                      (fun parent -> removeChild parent node)
     else ();
     fun t -> substop t; stop ()
  | Linked.Text (_, _, (_, stop)) -> stop (* removeChild fails on texts *)

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

let extract_linked_node = function
  | Linked.Node (_, _, _, _, _, (node, _)) ->
     (node : Dom_html.element Js.t :> Dom.node Js.t)
  | Linked.Text (_, _, (node, _)) -> (node : Dom.text Js.t :> Dom.node Js.t)

let rec diff_and_patch_opt parent vdom_opt vdom_opt' =
  match vdom_opt, vdom_opt' with
  | None, Some vdom' ->
     let (vdom'', callback) = link parent vdom' in
     (Some vdom'', callback, true)
  | Some vdom, None ->
     let callback = unlink vdom in (None, callback, true)
  | None, None -> assert false

  | Some (Linked.Node (name, id, attrs, children, strategy,
                       (node, stop))),
    Some (Node (name', id', attrs', children', strategy', start))
       when name = name' && id = id' ->
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

     let (stop', full_callback) =
       if restart_needed strategy strategy' situation
       then let start' node = stop (); start node in
            let (stop, callback) = make_callback_functions start' node in
            (stop, fun t -> subcallback t; callback t)
       else (stop, fun t -> subcallback t)
     in
     let linked_node = Linked.Node (name, id, attrs', children'', strategy',
                                    (node, stop')) in

     (Some linked_node, full_callback, changed)

  | Some (Linked.Text (str, strategy, (node, stop))),
    Some (Text (str', strategy', start)) when str != str' ->
     let _ = node##.data := Js.string str' in

     let (stop', callback) =
       if restart_needed strategy strategy' `CurrentNodeChanged
       then let start' node = stop (); start node in
            make_callback_functions start' node
       else (stop, fun _ -> ())
     in

     let linked_node = Linked.Text (str', strategy', (node, stop')) in
     (Some linked_node, callback, false) (* text change = no change *)

  | Some (Linked.Text (str, strategy, (node, stop))),
    Some (Text (_, strategy', start)) ->
     let (stop', callback) =
       if restart_needed strategy strategy' `NothingChanged
       then let start' node = stop (); start node in
            make_callback_functions start' node
       else (stop, fun () -> ())
     in
     let linked_node = Linked.Text (str, strategy, (node, stop')) in
     (Some linked_node, callback, false)

  | Some vdom, Some vdom' ->
     let callback = unlink ~remove:false vdom in
     let (vdom'', callback') =
       link ~current:(extract_linked_node vdom) parent vdom'
     in
     let callback'' t = callback t; callback' t in
     (Some vdom'', callback'', true)

let diff_and_patch parent vdom vdom' =
  let (vdom_opt, callback, _) =
    diff_and_patch_opt parent (Some vdom) (Some vdom')
  in match vdom_opt with
     | Some vdom'' -> (vdom'', callback)
     | None -> callback (); assert false

let vdom parent signal f =
  let stopref = ref (fun () -> ()) in
  let redraw vdom_opt (t, x) =
    let vdom' = f x in
    let (linked, callback) = match vdom_opt with
      | None      -> link parent vdom'
      | Some vdom -> diff_and_patch parent vdom vdom'
    in
    stopref := (fun () -> unlink linked ());
    (Some linked, callback)
  in

  (* force:true to draw once even though there are no events *)
  perform_state_post ~force:true ~init:None ~f:redraw
                     ((fun x y -> (x, y)) <$> time <*> signal);

  fun () -> !stopref ()

let vdom_ parent f = vdom parent (pure ()) f

(* Helpers for specific elements *)
module Element = struct
  let a ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.a node)
                          (fun () -> assert false) network
            ) "a"

  let area ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.area node)
                          (fun () -> assert false) network
            ) "area"

  let article ?strategy ?id network =
    element ?strategy ?id ~network "article"

  let base ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.base node)
                          (fun () -> assert false) network
            ) "base"

  let blockquote ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.blockquote node)
                          (fun () -> assert false) network
            ) "blockquote"

  let body ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.body node)
                          (fun () -> assert false) network
            ) "body"

  let br ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.br node)
                          (fun () -> assert false) network
            ) "br"

  let button ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.button node)
                          (fun () -> assert false) network
            ) "button"

  let canvas ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.canvas node)
                          (fun () -> assert false) network
            ) "canvas"

  let caption ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.caption node)
                          (fun () -> assert false) network
            ) "caption"

  let col ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.col node)
                          (fun () -> assert false) network
            ) "col"

  let colgroup ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.colgroup node)
                          (fun () -> assert false) network
            ) "colgroup"

  let del ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.del node)
                          (fun () -> assert false) network
            ) "del"

  let div ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.div node)
                          (fun () -> assert false) network
            ) "div"

  let dl ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.dl node)
                          (fun () -> assert false) network
            ) "dl"

  let fieldset ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.fieldset node)
                          (fun () -> assert false) network
            ) "fieldset"

  let embed ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.embed node)
                          (fun () -> assert false) network
            ) "embed"

  let form ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.form node)
                          (fun () -> assert false) network
            ) "form"

  let frameset ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.frameset node)
                          (fun () -> assert false) network
            ) "frameset"

  let frame ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.frame node)
                          (fun () -> assert false) network
            ) "frame"

  let h1 ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h1 node)
                          (fun () -> assert false) network
            ) "h1"

  let h2 ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h2 node)
                          (fun () -> assert false) network
            ) "h2"

  let h3 ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h3 node)
                          (fun () -> assert false) network
            ) "h3"

  let h4 ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h4 node)
                          (fun () -> assert false) network
            ) "h4"

  let h5 ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h5 node)
                          (fun () -> assert false) network
            ) "h5"

  let h6 ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h6 node)
                          (fun () -> assert false) network
            ) "h6"

  let head ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.head node)
                          (fun () -> assert false) network
            ) "head"

  let header ?strategy ?id network =
    element ?strategy ?id ~network "header"

  let hr ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.hr node)
                          (fun () -> assert false) network
            ) "hr"

  let html ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.html node)
                          (fun () -> assert false) network
            ) "html"

  let iframe ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.iframe node)
                          (fun () -> assert false) network
            ) "iframe"

  let img ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.img node)
                          (fun () -> assert false) network
            ) "img"

  let input ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.input node)
                          (fun () -> assert false) network
            ) "input"

  let ins ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ins node)
                          (fun () -> assert false) network
            ) "ins"

  let label ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.label node)
                          (fun () -> assert false) network
            ) "label"

  let legend ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.legend node)
                          (fun () -> assert false) network
            ) "legend"

  let li ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.li node)
                          (fun () -> assert false) network
            ) "li"

  let link ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.link node)
                          (fun () -> assert false) network
            ) "link"

  let map ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.map node)
                          (fun () -> assert false) network
            ) "map"

  let meta ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.meta node)
                          (fun () -> assert false) network
            ) "meta"

  let nav ?strategy ?id network =
    element ?strategy ?id ~network "nav"

  let _object ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo._object node)
                          (fun () -> assert false) network
            ) "object"

  let ol ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ol node)
                          (fun () -> assert false) network
            ) "ol"

  let optgroup ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.optgroup node)
                          (fun () -> assert false) network
            ) "optgroup"

  let option ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.option node)
                          (fun () -> assert false) network
            ) "option"

  let p ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.p node)
                          (fun () -> assert false) network
            ) "p"

  let param ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.param node)
                          (fun () -> assert false) network
            ) "param"

  let pre ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.pre node)
                          (fun () -> assert false) network
            ) "pre"

  let q ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.q node)
                          (fun () -> assert false) network
            ) "q"

  let script ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.script node)
                          (fun () -> assert false) network
            ) "script"

  let section ?strategy ?id network =
    element ?strategy ?id ~network "section"

  let select ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.select node)
                          (fun () -> assert false) network
            ) "select"

  let style ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.style node)
                          (fun () -> assert false) network
            ) "style"

  let table ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.table node)
                          (fun () -> assert false) network
            ) "table"

  let tbody ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tbody node)
                          (fun () -> assert false) network
            ) "tbody"

  let td ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.td node)
                          (fun () -> assert false) network
            ) "td"

  let textarea ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.textarea node)
                          (fun () -> assert false) network
            ) "textarea"

  let tfoot ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tfoot node)
                          (fun () -> assert false) network
            ) "tfoot"

  let th ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.th node)
                          (fun () -> assert false) network
            ) "th"

  let thead ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.thead node)
                          (fun () -> assert false) network
            ) "thead"

  let title ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.title node)
                          (fun () -> assert false) network
            ) "title"

  let tr ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tr node)
                          (fun () -> assert false) network
            ) "tr"

  let ul ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ul node)
                          (fun () -> assert false) network
            ) "ul"

  let audio ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.audio node)
                          (fun () -> assert false) network
            ) "audio"

  let video ?strategy ?id network =
    element ?strategy ?id ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.video node)
                          (fun () -> assert false) network
            ) "video"
end

module E = Element
