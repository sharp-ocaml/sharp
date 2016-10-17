open Sharp_core

open Dom
open Dom_html

type attributes = (string * string) list

module type Extra = sig
  type 'a t
end

module Generic (E : Extra) = struct
  type t =
    | Node of string * attributes * t list * element Js.t E.t
    | Text of string * text Js.t E.t

  let append_child child parent = match parent with
    | Node (name, attrs, children, extra) ->
       Node (name, attrs, children @ [child], extra)
    | Text _ as t -> t

  let prepend_child child parent = match parent with
    | Node (name, attrs, children, extra) ->
       Node (name, attrs, child :: children, extra)
    | Text _ as t -> t

  let set_attribute attr_name attr_value = function
    | Node (name, attrs, children, extra) ->
       let attrs' = (attr_name, attr_value) :: List.remove_assoc name attrs in
       Node (name, attrs', children, extra)
    | Text _ as t -> t

  let clear_attribute name = function
    | Node (name, attrs, children, extra) ->
       let attrs' = List.remove_assoc name attrs in
       Node (name, attrs', children, extra)
    | Text _ as t -> t

  let eq vdom vdom' = match vdom, vdom' with
    | Node (name, attrs, _, _), Node (name', attrs', _, _) ->
       name = name' && attrs = attrs'
    | Text (t, _), Text (t', _) -> t = t'
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

let node ?network name children =
  let f = match network with
    | None     -> Raw.empty
    | Some net -> fun node -> Network.start (net node)
  in
  Node (name, [], children, f)
let element ?network name = node ?network name []
let tag = element
let text content = Text (content, Raw.empty)

let rec link ?current parent vdom = match vdom with
  | Node (name, attrs, children, f) ->
     let node = document##createElement (Js.string name) in
     let _    =
       List.map (fun (n,v) -> node##setAttribute (Js.string n) (Js.string v))
                attrs
     in
     let _ = match current with
       | None   -> appendChild  parent node
       | Some n -> replaceChild parent node n
     in
     let children' = List.map (link node) children in
     (* children created before initialisation *)
     let g         = f node in
     Linked.Node (name, attrs, children', (node, g))

  | Text (str, f) ->
     let node = document##createTextNode (Js.string str) in
     let _ = match current with
       | None   -> appendChild  parent node
       | Some n -> replaceChild parent node n
     in
     let g = f node in
     Linked.Text (str, (node, g))

let rec unlink vdom = match vdom with
  | Linked.Node (name, attrs, children, (node, f)) ->
     let () = f () in
     let _  = List.map unlink children in
     Js.Opt.iter (node##.parentNode)
                 (fun parent -> removeChild parent node)
  | Linked.Text (str, (node, f)) -> f () (* removeChild fails on texts *)

let rec update_attributes node old_attrs new_attrs =
  match old_attrs, new_attrs with
  | [], [] -> false
  | attrs, [] | [], attrs ->
     let f name value = node##setAttribute (Js.string name) (Js.string value) in
     let _ = List.iter (fun (n,v) -> f n v) attrs in
     true
  | (name, value) :: old_attrs', _ ->
     match List.partition (fun (n,_) -> n = name) new_attrs with
     | [], _ ->
        let _ = node##removeAttribute (Js.string name) in
        let _ = update_attributes node old_attrs' new_attrs in
        true
     | (_, value') :: _, new_attrs' when value = value' ->
        update_attributes node old_attrs' new_attrs'
     | (_, value') :: _, new_attrs' ->
        let _ = node##setAttribute (Js.string name) (Js.string value') in
        let _ = update_attributes node old_attrs' new_attrs' in
        true

let rec fold_left2_opt ~f acc xs ys = match xs, ys with
  | [], [] -> acc
  | x :: xs', [] ->
     let acc' = f acc (Some x) None in fold_left2_opt ~f acc' xs' []
  | [], y :: ys' ->
     let acc' = f acc None (Some y) in fold_left2_opt ~f acc' [] ys'
  | x :: xs', y :: ys' ->
     let acc' = f acc (Some x) (Some y) in fold_left2_opt ~f acc' xs' ys'

let rec diff_and_patch_opt parent vdom_opt vdom_opt' =
  match vdom_opt, vdom_opt' with
  | None, Some vdom' -> Some (link parent vdom', true)
  | Some vdom, None  ->
     let _ = unlink vdom in None
  | None, None       -> assert false

  | Some (Linked.Node (name, attrs, children, (node, f))),
    Some (Node (name', attrs', children', g)) when name = name' ->
     let node_changed = update_attributes node attrs attrs' in
     let (children'', children_changed) =
       fold_left2_opt ([], false) children children'
                       ~f:(fun acc child child' ->
                         let (children'', changed) = acc in
                         let child_opt =
                           diff_and_patch_opt node child child'
                         in match child_opt with
                            | None -> (children'', true)
                            | Some (child'', changed') ->
                               (children'' @ [child''], changed || changed')
                       )
     in
     let _ = f () in
     let f' = g node in
     let changed = node_changed || children_changed in
     Some (Linked.Node (name, attrs', children'', (node, f')), changed)

  | Some (Linked.Text (str, (node, f))),
    Some (Text (str', g)) when str != str' ->
     let _ = node##.data := Js.string str' in
     let _ = f () in
     let f' = g node in
     Some (Linked.Text (str', (node, f')), false) (* text change = no change *)

  | Some (Linked.Text _ as t), Some (Text _) -> Some (t, false)

  | Some vdom, Some vdom' ->
     let _ = unlink vdom in
     Some (link parent vdom', true)

let diff_and_patch parent vdom vdom' =
  let vdom_opt = diff_and_patch_opt parent (Some vdom) (Some vdom') in
  match vdom_opt with
  | Some (vdom'', _) -> vdom''
  | None -> assert false

let vdom parent b f =
  let open Network in
  let open Network.Infix in
  let finaliser_ref = ref (fun () -> ()) in
  let rec g vdom_opt x =
    let vdom' = f x in
    let linked = match vdom_opt with
      | None      -> link parent vdom'
      | Some vdom -> diff_and_patch parent vdom vdom'
    in Some linked
  in
  perform_state b ~init:None ~f:g >>
  finally (fun () -> (!finaliser_ref) ())

(* Helpers for specific elements *)
module Element = struct
  let a network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.a node)
                          (fun () -> assert false) network
            ) "a"

  let area network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.area node)
                          (fun () -> assert false) network
            ) "area"

  let base network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.base node)
                          (fun () -> assert false) network
            ) "base"

  let blockquote network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.blockquote node)
                          (fun () -> assert false) network
            ) "blockquote"

  let body network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.body node)
                          (fun () -> assert false) network
            ) "body"

  let br network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.br node)
                          (fun () -> assert false) network
            ) "br"

  let button network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.button node)
                          (fun () -> assert false) network
            ) "button"

  let canvas network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.canvas node)
                          (fun () -> assert false) network
            ) "canvas"

  let caption network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.caption node)
                          (fun () -> assert false) network
            ) "caption"

  let col network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.col node)
                          (fun () -> assert false) network
            ) "col"

  let colgroup network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.colgroup node)
                          (fun () -> assert false) network
            ) "colgroup"

  let del network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.del node)
                          (fun () -> assert false) network
            ) "del"

  let div network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.div node)
                          (fun () -> assert false) network
            ) "div"

  let dl network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.dl node)
                          (fun () -> assert false) network
            ) "dl"

  let fieldset network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.fieldset node)
                          (fun () -> assert false) network
            ) "fieldset"

  let embed network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.embed node)
                          (fun () -> assert false) network
            ) "embed"

  let form network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.form node)
                          (fun () -> assert false) network
            ) "form"

  let frameset network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.frameset node)
                          (fun () -> assert false) network
            ) "frameset"

  let frame network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.frame node)
                          (fun () -> assert false) network
            ) "frame"

  let h1 network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h1 node)
                          (fun () -> assert false) network
            ) "h1"

  let h2 network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h2 node)
                          (fun () -> assert false) network
            ) "h2"

  let h3 network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h3 node)
                          (fun () -> assert false) network
            ) "h3"

  let h4 network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h4 node)
                          (fun () -> assert false) network
            ) "h4"

  let h5 network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h5 node)
                          (fun () -> assert false) network
            ) "h5"

  let h6 network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.h6 node)
                          (fun () -> assert false) network
            ) "h6"

  let head network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.head node)
                          (fun () -> assert false) network
            ) "head"

  let hr network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.hr node)
                          (fun () -> assert false) network
            ) "hr"

  let html network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.html node)
                          (fun () -> assert false) network
            ) "html"

  let iframe network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.iframe node)
                          (fun () -> assert false) network
            ) "iframe"

  let img network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.img node)
                          (fun () -> assert false) network
            ) "img"

  let input network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.input node)
                          (fun () -> assert false) network
            ) "input"

  let ins network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ins node)
                          (fun () -> assert false) network
            ) "ins"

  let label network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.label node)
                          (fun () -> assert false) network
            ) "label"

  let legend network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.legend node)
                          (fun () -> assert false) network
            ) "legend"

  let li network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.li node)
                          (fun () -> assert false) network
            ) "li"

  let link network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.link node)
                          (fun () -> assert false) network
            ) "link"

  let map network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.map node)
                          (fun () -> assert false) network
            ) "map"

  let meta network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.meta node)
                          (fun () -> assert false) network
            ) "meta"

  let _object network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo._object node)
                          (fun () -> assert false) network
            ) "object"

  let ol network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ol node)
                          (fun () -> assert false) network
            ) "ol"

  let optgroup network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.optgroup node)
                          (fun () -> assert false) network
            ) "optgroup"

  let option network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.option node)
                          (fun () -> assert false) network
            ) "option"

  let p network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.p node)
                          (fun () -> assert false) network
            ) "p"

  let param network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.param node)
                          (fun () -> assert false) network
            ) "param"

  let pre network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.pre node)
                          (fun () -> assert false) network
            ) "pre"

  let q network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.q node)
                          (fun () -> assert false) network
            ) "q"

  let script network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.script node)
                          (fun () -> assert false) network
            ) "script"

  let select network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.select node)
                          (fun () -> assert false) network
            ) "select"

  let style network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.style node)
                          (fun () -> assert false) network
            ) "style"

  let table network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.table node)
                          (fun () -> assert false) network
            ) "table"

  let tbody network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tbody node)
                          (fun () -> assert false) network
            ) "tbody"

  let td network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.td node)
                          (fun () -> assert false) network
            ) "td"

  let textarea network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.textarea node)
                          (fun () -> assert false) network
            ) "textarea"

  let tfoot network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tfoot node)
                          (fun () -> assert false) network
            ) "tfoot"

  let th network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.th node)
                          (fun () -> assert false) network
            ) "th"

  let thead network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.thead node)
                          (fun () -> assert false) network
            ) "thead"

  let title network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.title node)
                          (fun () -> assert false) network
            ) "title"

  let tr network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.tr node)
                          (fun () -> assert false) network
            ) "tr"

  let ul network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.ul node)
                          (fun () -> assert false) network
            ) "ul"

  let audio network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.audio node)
                          (fun () -> assert false) network
            ) "audio"

  let video network =
    element ~network:(fun node ->
              Js.Opt.case (Dom_html.CoerceTo.video node)
                          (fun () -> assert false) network
            ) "video"
end

module E = Element
