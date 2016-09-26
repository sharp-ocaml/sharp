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
  react b ~init:None ~f:g >>
  finally (fun () -> (!finaliser_ref) ())
