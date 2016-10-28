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
    | Node of string * attributes * t list * restart_strategy * element Js.t E.t
    | Text of string * restart_strategy * text Js.t E.t

  let append_child child parent = match parent with
    | Node (name, attrs, children, strategy, extra) ->
       Node (name, attrs, children @ [child], strategy, extra)
    | Text _ as t -> t

  let prepend_child child parent = match parent with
    | Node (name, attrs, children, strategy, extra) ->
       Node (name, attrs, child :: children, strategy, extra)
    | Text _ as t -> t

  let set_attribute attr_name attr_value = function
    | Node (name, attrs, children, strategy, extra) ->
       let attrs' = (attr_name, attr_value) :: List.remove_assoc name attrs in
       Node (name, attrs', children, strategy, extra)
    | Text _ as t -> t

  let clear_attribute name = function
    | Node (name, attrs, children, strategy, extra) ->
       let attrs' = List.remove_assoc name attrs in
       Node (name, attrs', children, strategy, extra)
    | Text _ as t -> t

  let eq vdom vdom' = match vdom, vdom' with
    | Node (name, attrs, _, strategy, _),
      Node (name', attrs', _, strategy', _) ->
       name = name' && attrs = attrs' && strategy = strategy'
    | Text (t, strategy, _), Text (t', strategy', _) ->
       t = t' && strategy = strategy'
    | _, _ -> false

  let ( |- ) parent child = parent |> append_child child
  let rec ( |+ ) parent = function
    | [] -> parent
    | child :: children -> append_child child parent |+ children
  let ( |* ) node (name, value) = node |> set_attribute name value
end

module Raw : sig
  type 'a t = 'a -> unit -> unit
  val empty : 'a t
end = struct
  type 'a t = 'a -> unit -> unit
  let empty _ () = ()
end

module LinkedExtra = struct
  type 'a t = 'a * (unit -> unit)
end

module Linked = Generic(LinkedExtra)
include Generic(Raw)

let node ?network ?(strategy=OnDeepChange) name children =
  let f = match network with
    | None     -> Raw.empty
    | Some net -> fun node -> Network.start (net node)
  in
  Node (name, [], children, strategy, f)
let element ?network ?strategy name = node ?network ?strategy name []
let tag = element
let text ?network ?(strategy=OnDeepChange) content =
  let f = match network with
    | None     -> Raw.empty
    | Some net -> fun node -> Network.start (net node)
  in Text (content, strategy, f)

let make_callback_functions f node =
  let linked = ref true in
  let gref   = ref (fun () -> linked := false) in
  let linked_function () = (!gref) () in
  let callback () = if !linked
                    then let unlink = f node in gref := unlink
                    else ()
  in (linked_function, callback)

let rec link ?current parent vdom = match vdom with
  | Node (name, attrs, children, strategy, f) ->
     let node = document##createElement (Js.string name) in
     let _    =
       List.map (fun (n,v) -> node##setAttribute (Js.string n) (Js.string v))
                attrs
     in
     let _ = match current with
       | None   -> appendChild  parent node
       | Some n -> replaceChild parent node n
     in
     let pairs          = List.map (link node) children in
     let children'      = List.map (fun (n,_) -> n) pairs in
     let subcallback () = List.iter (fun (_,c) -> c ()) pairs in
     (* children created before initialisation *)
     let (linked_function, callback) = make_callback_functions f node in
     let linked_node =
       Linked.Node (name, attrs, children', strategy, (node, linked_function))
     in
     (linked_node, fun () -> subcallback (); callback ())

  | Text (str, strategy, f) ->
     let node = document##createTextNode (Js.string str) in
     let _ = match current with
       | None   -> appendChild  parent node
       | Some n -> replaceChild parent node n
     in
     let (linked_function, callback) = make_callback_functions f node in
     (Linked.Text (str, strategy, (node, linked_function)), callback)

let rec unlink vdom = match vdom with
  | Linked.Node (_, _, children, _, (node, callback)) ->
     let subcallbacks   = List.map unlink children in
     let subcallback () = List.iter (fun f -> f ()) subcallbacks in
     Js.Opt.iter (node##.parentNode)
                 (fun parent -> removeChild parent node);
     fun () -> subcallback (); callback ()
  | Linked.Text (_, _, (_, callback)) ->
     callback (* removeChild fails on texts *)

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
  | Some vdom, None  -> let callback = unlink vdom in (None, callback, true)
  | None, None       -> assert false

  | Some (Linked.Node (name, attrs, children, strategy, (node, f))),
    Some (Node (name', attrs', children', strategy', g)) when name = name' ->
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
     let subcallback () = List.iter (fun k -> k ()) subcallbacks in

     let changed = node_changed || children_changed in
     let situation = match node_changed, children_changed with
       | true,  _     -> `CurrentNodeChanged
       | false, true  -> `OnlyChildrenChanged
       | false, false -> `NothingChanged
     in

     let (linked_function, full_callback) =
       if restart_needed strategy strategy' situation
       then let g' node = f (); g node in
            let (linked_function, callback) = make_callback_functions g' node in
            (linked_function, fun () -> subcallback (); callback ())
       else (f, subcallback)
     in
     let linked_node = Linked.Node (name, attrs', children'', strategy',
                                    (node, linked_function)) in

     (Some linked_node, full_callback, changed)

  | Some (Linked.Text (str, strategy, (node, f))),
    Some (Text (str', strategy', g)) when str != str' ->
     let _ = node##.data := Js.string str' in

     let (linked_function, callback) =
       if restart_needed strategy strategy' `CurrentNodeChanged
       then let g' node = f (); g node in make_callback_functions g' node
       else (f, fun () -> ())
     in

     let linked_node = Linked.Text (str', strategy', (node, linked_function)) in
     (Some linked_node, callback, false) (* text change = no change *)

  | Some (Linked.Text (str, strategy, (node, f))),
    Some (Text (_, strategy', g)) ->
     let (linked_function, callback) =
       if restart_needed strategy strategy' `NothingChanged
       then let g' node = f (); g node in make_callback_functions g' node
       else (f, fun () -> ())
     in
     let linked_node = Linked.Text (str, strategy, (node, linked_function)) in
     (Some linked_node, callback, false)

  | Some vdom, Some vdom' ->
     let callback = unlink vdom in
     let (vdom'', callback') = link parent vdom' in
     let callback'' () = callback (); callback' () in
     (Some vdom'', callback'', true)

let diff_and_patch parent vdom vdom' =
  let (vdom_opt, callback, _) =
    diff_and_patch_opt parent (Some vdom) (Some vdom')
  in match vdom_opt with
     | Some vdom'' -> (vdom'', callback)
     | None -> callback (); assert false

let vdom parent b f =
  let open Network in
  let open Network.Infix in
  let rec g vdom_opt x =
    let vdom' = f x in
    let (linked, callback) = match vdom_opt with
      | None      -> link parent vdom'
      | Some vdom -> diff_and_patch parent vdom vdom'
    in (Some linked, callback)
  in
  perform_state_post ~init:None ~f:g
                     ~finally:(function
                               | None -> ()
                               | Some vdom -> unlink vdom ()
                              ) b

(* Helpers for specific elements *)
module Element = struct
  let a ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.a node)
                          (fun () -> assert false) network
            ) "a"

  let area ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.area node)
                          (fun () -> assert false) network
            ) "area"

  let article ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.div node)
                          (fun () -> assert false) network
            ) "article"

  let base ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.base node)
                          (fun () -> assert false) network
            ) "base"

  let blockquote ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.blockquote node)
                          (fun () -> assert false) network
            ) "blockquote"

  let body ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.body node)
                          (fun () -> assert false) network
            ) "body"

  let br ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.br node)
                          (fun () -> assert false) network
            ) "br"

  let button ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.button node)
                          (fun () -> assert false) network
            ) "button"

  let canvas ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.canvas node)
                          (fun () -> assert false) network
            ) "canvas"

  let caption ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.caption node)
                          (fun () -> assert false) network
            ) "caption"

  let col ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.col node)
                          (fun () -> assert false) network
            ) "col"

  let colgroup ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.colgroup node)
                          (fun () -> assert false) network
            ) "colgroup"

  let del ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.del node)
                          (fun () -> assert false) network
            ) "del"

  let div ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.div node)
                          (fun () -> assert false) network
            ) "div"

  let dl ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.dl node)
                          (fun () -> assert false) network
            ) "dl"

  let fieldset ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.fieldset node)
                          (fun () -> assert false) network
            ) "fieldset"

  let embed ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.embed node)
                          (fun () -> assert false) network
            ) "embed"

  let form ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.form node)
                          (fun () -> assert false) network
            ) "form"

  let frameset ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.frameset node)
                          (fun () -> assert false) network
            ) "frameset"

  let frame ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.frame node)
                          (fun () -> assert false) network
            ) "frame"

  let h1 ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h1 node)
                          (fun () -> assert false) network
            ) "h1"

  let h2 ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h2 node)
                          (fun () -> assert false) network
            ) "h2"

  let h3 ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h3 node)
                          (fun () -> assert false) network
            ) "h3"

  let h4 ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h4 node)
                          (fun () -> assert false) network
            ) "h4"

  let h5 ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h5 node)
                          (fun () -> assert false) network
            ) "h5"

  let h6 ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h6 node)
                          (fun () -> assert false) network
            ) "h6"

  let head ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.head node)
                          (fun () -> assert false) network
            ) "head"

  let header ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.div node)
                          (fun () -> assert false) network
            ) "header"

  let hr ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.hr node)
                          (fun () -> assert false) network
            ) "hr"

  let html ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.html node)
                          (fun () -> assert false) network
            ) "html"

  let iframe ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.iframe node)
                          (fun () -> assert false) network
            ) "iframe"

  let img ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.img node)
                          (fun () -> assert false) network
            ) "img"

  let input ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.input node)
                          (fun () -> assert false) network
            ) "input"

  let ins ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ins node)
                          (fun () -> assert false) network
            ) "ins"

  let label ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.label node)
                          (fun () -> assert false) network
            ) "label"

  let legend ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.legend node)
                          (fun () -> assert false) network
            ) "legend"

  let li ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.li node)
                          (fun () -> assert false) network
            ) "li"

  let link ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.link node)
                          (fun () -> assert false) network
            ) "link"

  let map ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.map node)
                          (fun () -> assert false) network
            ) "map"

  let meta ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.meta node)
                          (fun () -> assert false) network
            ) "meta"

  let nav ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.div node)
                          (fun () -> assert false) network
            ) "nav"

  let _object ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo._object node)
                          (fun () -> assert false) network
            ) "object"

  let ol ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ol node)
                          (fun () -> assert false) network
            ) "ol"

  let optgroup ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.optgroup node)
                          (fun () -> assert false) network
            ) "optgroup"

  let option ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.option node)
                          (fun () -> assert false) network
            ) "option"

  let p ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.p node)
                          (fun () -> assert false) network
            ) "p"

  let param ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.param node)
                          (fun () -> assert false) network
            ) "param"

  let pre ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.pre node)
                          (fun () -> assert false) network
            ) "pre"

  let q ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.q node)
                          (fun () -> assert false) network
            ) "q"

  let script ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.script node)
                          (fun () -> assert false) network
            ) "script"

  let section ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.div node)
                          (fun () -> assert false) network
            ) "section"

  let select ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.select node)
                          (fun () -> assert false) network
            ) "select"

  let style ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.style node)
                          (fun () -> assert false) network
            ) "style"

  let table ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.table node)
                          (fun () -> assert false) network
            ) "table"

  let tbody ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tbody node)
                          (fun () -> assert false) network
            ) "tbody"

  let td ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.td node)
                          (fun () -> assert false) network
            ) "td"

  let textarea ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.textarea node)
                          (fun () -> assert false) network
            ) "textarea"

  let tfoot ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tfoot node)
                          (fun () -> assert false) network
            ) "tfoot"

  let th ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.th node)
                          (fun () -> assert false) network
            ) "th"

  let thead ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.thead node)
                          (fun () -> assert false) network
            ) "thead"

  let title ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.title node)
                          (fun () -> assert false) network
            ) "title"

  let tr ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tr node)
                          (fun () -> assert false) network
            ) "tr"

  let ul ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ul node)
                          (fun () -> assert false) network
            ) "ul"

  let audio ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.audio node)
                          (fun () -> assert false) network
            ) "audio"

  let video ?strategy network =
    element ?strategy ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.video node)
                          (fun () -> assert false) network
            ) "video"
end

module E = Element
