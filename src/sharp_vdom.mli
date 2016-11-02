open Sharp_core

type t
type attributes
type restart_strategy =
  | Never | Always | OnChange | OnDeepChange | OnIdentifierChange of string

module Linked : sig
  type t
end

val node    : ?network:(Dom_html.element Js.t -> unit Network.t)
              -> ?strategy:restart_strategy -> string -> t list -> t
val element : ?network:(Dom_html.element Js.t -> unit Network.t)
              -> ?strategy:restart_strategy-> string -> t
val tag     : ?network:(Dom_html.element Js.t -> unit Network.t)
              -> ?strategy:restart_strategy -> string -> t
val text    : ?network:(Dom.text Js.t -> unit Network.t)
              -> ?strategy:restart_strategy -> string -> t

val append_child  : t -> t -> t
val prepend_child : t -> t -> t

val set_attribute : string -> string -> t -> t
val clear_attribute : string -> t -> t

val ( |- ) : t -> t -> t
val ( |+ ) : t -> t list -> t
val ( |* ) : t -> string * string -> t

val link : ?current:Dom.node Js.t -> Dom_html.element Js.t -> t
           -> Linked.t * (unit -> unit)
(** Link a VDOM to a node and return a pair with a linked VDOM and a callback to
    start the subnetworks.
    This is so that we can use perform_state_post to start the subnetworks after
    the new state has been recorded. *)

val unlink : Linked.t -> (unit -> unit)
val diff_and_patch : Dom_html.element Js.t -> Linked.t -> t
                     -> Linked.t * (time -> unit)

val vdom : Dom_html.element Js.t -> ('a, 'b) Behaviour.t -> ('a -> t)
           -> unit Network.t
val vdom_ : Dom_html.element Js.t -> (unit -> t) -> unit Network.t

(* Helpers for specific elements *)
module Element : sig
  val a          : ?strategy:restart_strategy
                   -> (Dom_html.anchorElement Js.t -> 'a Network.t) -> t
  val area       : ?strategy:restart_strategy
                   -> (Dom_html.areaElement Js.t -> 'a Network.t) -> t
  val article    : ?strategy:restart_strategy
                   -> (Dom_html.element Js.t -> 'a Network.t) -> t
  val base       : ?strategy:restart_strategy
                   -> (Dom_html.baseElement Js.t -> 'a Network.t) -> t
  val blockquote : ?strategy:restart_strategy
                   -> (Dom_html.quoteElement Js.t -> 'a Network.t) -> t
  val body       : ?strategy:restart_strategy
                   -> (Dom_html.bodyElement Js.t -> 'a Network.t) -> t
  val br         : ?strategy:restart_strategy
                   -> (Dom_html.brElement Js.t -> 'a Network.t) -> t
  val button     : ?strategy:restart_strategy
                   -> (Dom_html.buttonElement Js.t -> 'a Network.t) -> t
  val canvas     : ?strategy:restart_strategy
                   -> (Dom_html.canvasElement Js.t -> 'a Network.t) -> t
  val caption    : ?strategy:restart_strategy
                   -> (Dom_html.tableCaptionElement Js.t -> 'a Network.t) -> t
  val col        : ?strategy:restart_strategy
                   -> (Dom_html.tableColElement Js.t -> 'a Network.t) -> t
  val colgroup   : ?strategy:restart_strategy
                   -> (Dom_html.tableColElement Js.t -> 'a Network.t) -> t
  val del        : ?strategy:restart_strategy
                   -> (Dom_html.modElement Js.t -> 'a Network.t) -> t
  val div        : ?strategy:restart_strategy
                   -> (Dom_html.divElement Js.t -> 'a Network.t) -> t
  val dl         : ?strategy:restart_strategy
                   -> (Dom_html.dListElement Js.t -> 'a Network.t) -> t
  val fieldset   : ?strategy:restart_strategy
                   -> (Dom_html.fieldSetElement Js.t -> 'a Network.t) -> t
  val embed      : ?strategy:restart_strategy
                   -> (Dom_html.embedElement Js.t -> 'a Network.t) -> t
  val form       : ?strategy:restart_strategy
                   -> (Dom_html.formElement Js.t -> 'a Network.t) -> t
  val frameset   : ?strategy:restart_strategy
                   -> (Dom_html.frameSetElement Js.t -> 'a Network.t) -> t
  val frame      : ?strategy:restart_strategy
                   -> (Dom_html.frameElement Js.t -> 'a Network.t) -> t
  val h1         : ?strategy:restart_strategy
                   -> (Dom_html.headingElement Js.t -> 'a Network.t) -> t
  val h2         : ?strategy:restart_strategy
                   -> (Dom_html.headingElement Js.t -> 'a Network.t) -> t
  val h3         : ?strategy:restart_strategy
                   -> (Dom_html.headingElement Js.t -> 'a Network.t) -> t
  val h4         : ?strategy:restart_strategy
                   -> (Dom_html.headingElement Js.t -> 'a Network.t) -> t
  val h5         : ?strategy:restart_strategy
                   -> (Dom_html.headingElement Js.t -> 'a Network.t) -> t
  val h6         : ?strategy:restart_strategy
                   -> (Dom_html.headingElement Js.t -> 'a Network.t) -> t
  val header     : ?strategy:restart_strategy
                   -> (Dom_html.element Js.t -> 'a Network.t) -> t
  val head       : ?strategy:restart_strategy
                   -> (Dom_html.headElement Js.t -> 'a Network.t) -> t
  val hr         : ?strategy:restart_strategy
                   -> (Dom_html.hrElement Js.t -> 'a Network.t) -> t
  val html       : ?strategy:restart_strategy
                   -> (Dom_html.htmlElement Js.t -> 'a Network.t) -> t
  val iframe     : ?strategy:restart_strategy
                   -> (Dom_html.iFrameElement Js.t -> 'a Network.t) -> t
  val img        : ?strategy:restart_strategy
                   -> (Dom_html.imageElement Js.t -> 'a Network.t) -> t
  val input      : ?strategy:restart_strategy
                   -> (Dom_html.inputElement Js.t -> 'a Network.t) -> t
  val ins        : ?strategy:restart_strategy
                   -> (Dom_html.modElement Js.t -> 'a Network.t) -> t
  val label      : ?strategy:restart_strategy
                   -> (Dom_html.labelElement Js.t -> 'a Network.t) -> t
  val legend     : ?strategy:restart_strategy
                   -> (Dom_html.legendElement Js.t -> 'a Network.t) -> t
  val li         : ?strategy:restart_strategy
                   -> (Dom_html.liElement Js.t -> 'a Network.t) -> t
  val link       : ?strategy:restart_strategy
                   -> (Dom_html.linkElement Js.t -> 'a Network.t) -> t
  val map        : ?strategy:restart_strategy
                   -> (Dom_html.mapElement Js.t -> 'a Network.t) -> t
  val meta       : ?strategy:restart_strategy
                   -> (Dom_html.metaElement Js.t -> 'a Network.t) -> t
  val nav        : ?strategy:restart_strategy
                   -> (Dom_html.element Js.t -> 'a Network.t) -> t
  val _object    : ?strategy:restart_strategy
                   -> (Dom_html.objectElement Js.t -> 'a Network.t) -> t
  val ol         : ?strategy:restart_strategy
                   -> (Dom_html.oListElement Js.t -> 'a Network.t) -> t
  val optgroup   : ?strategy:restart_strategy
                   -> (Dom_html.optGroupElement Js.t -> 'a Network.t) -> t
  val option     : ?strategy:restart_strategy
                   -> (Dom_html.optionElement Js.t -> 'a Network.t) -> t
  val p          : ?strategy:restart_strategy
                   -> (Dom_html.paramElement Js.t -> 'a Network.t) -> t
  val param      : ?strategy:restart_strategy
                   -> (Dom_html.paramElement Js.t -> 'a Network.t) -> t
  val pre        : ?strategy:restart_strategy
                   -> (Dom_html.preElement Js.t -> 'a Network.t) -> t
  val q          : ?strategy:restart_strategy
                   -> (Dom_html.quoteElement Js.t -> 'a Network.t) -> t
  val script     : ?strategy:restart_strategy
                   -> (Dom_html.scriptElement Js.t -> 'a Network.t) -> t
  val section    : ?strategy:restart_strategy
                   -> (Dom_html.element Js.t -> 'a Network.t) -> t
  val select     : ?strategy:restart_strategy
                   -> (Dom_html.selectElement Js.t -> 'a Network.t) -> t
  val style      : ?strategy:restart_strategy
                   -> (Dom_html.styleElement Js.t -> 'a Network.t) -> t
  val table      : ?strategy:restart_strategy
                   -> (Dom_html.tableElement Js.t -> 'a Network.t) -> t
  val tbody      : ?strategy:restart_strategy
                   -> (Dom_html.tableSectionElement Js.t -> 'a Network.t) -> t
  val td         : ?strategy:restart_strategy
                   -> (Dom_html.tableCellElement Js.t -> 'a Network.t) -> t
  val textarea   : ?strategy:restart_strategy
                   -> (Dom_html.textAreaElement Js.t -> 'a Network.t) -> t
  val tfoot      : ?strategy:restart_strategy
                   -> (Dom_html.tableSectionElement Js.t -> 'a Network.t) -> t
  val th         : ?strategy:restart_strategy
                   -> (Dom_html.tableCellElement Js.t -> 'a Network.t) -> t
  val thead      : ?strategy:restart_strategy
                   -> (Dom_html.tableSectionElement Js.t -> 'a Network.t) -> t
  val title      : ?strategy:restart_strategy
                   -> (Dom_html.titleElement Js.t -> 'a Network.t) -> t
  val tr         : ?strategy:restart_strategy
                   -> (Dom_html.tableRowElement Js.t -> 'a Network.t) -> t
  val ul         : ?strategy:restart_strategy
                   -> (Dom_html.uListElement Js.t -> 'a Network.t) -> t
  val audio      : ?strategy:restart_strategy
                   -> (Dom_html.audioElement Js.t -> 'a Network.t) -> t
  val video      : ?strategy:restart_strategy
                   -> (Dom_html.videoElement Js.t -> 'a Network.t) -> t
end

module E = Element
