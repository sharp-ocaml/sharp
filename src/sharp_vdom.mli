open Sharp_core

open Dom
open Dom_html

type t
type attributes

module Linked : sig
  type t
end

val node    : ?network:(element Js.t -> unit Network.t) -> string -> t list -> t
val element : ?network:(element Js.t -> unit Network.t) -> string -> t
val tag     : ?network:(element Js.t -> unit Network.t) -> string -> t
val text    : string -> t

val append_child  : t -> t -> t
val prepend_child : t -> t -> t

val set_attribute : string -> string -> t -> t
val clear_attribute : string -> t -> t

val ( |- ) : t -> t -> t
val ( |+ ) : t -> t list -> t
val ( |* ) : t -> string * string -> t

val link : ?current:node Js.t -> element Js.t -> t -> Linked.t
val unlink : Linked.t -> unit
val diff_and_patch : element Js.t -> Linked.t -> t -> Linked.t

val vdom : element Js.t -> 'a Behaviour.t -> ('a -> t) -> unit Network.t
