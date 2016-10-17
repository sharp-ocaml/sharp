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

(* Helpers for specific elements *)
module Element : sig
  val a          : (Dom_html.anchorElement       Js.t -> 'a Network.t) -> t
  val area       : (Dom_html.areaElement         Js.t -> 'a Network.t) -> t
  val base       : (Dom_html.baseElement         Js.t -> 'a Network.t) -> t
  val blockquote : (Dom_html.quoteElement        Js.t -> 'a Network.t) -> t
  val body       : (Dom_html.bodyElement         Js.t -> 'a Network.t) -> t
  val br         : (Dom_html.brElement           Js.t -> 'a Network.t) -> t
  val button     : (Dom_html.buttonElement       Js.t -> 'a Network.t) -> t
  val canvas     : (Dom_html.canvasElement       Js.t -> 'a Network.t) -> t
  val caption    : (Dom_html.tableCaptionElement Js.t -> 'a Network.t) -> t
  val col        : (Dom_html.tableColElement     Js.t -> 'a Network.t) -> t
  val colgroup   : (Dom_html.tableColElement     Js.t -> 'a Network.t) -> t
  val del        : (Dom_html.modElement          Js.t -> 'a Network.t) -> t
  val div        : (Dom_html.divElement          Js.t -> 'a Network.t) -> t
  val dl         : (Dom_html.dListElement        Js.t -> 'a Network.t) -> t
  val fieldset   : (Dom_html.fieldSetElement     Js.t -> 'a Network.t) -> t
  val embed      : (Dom_html.embedElement        Js.t -> 'a Network.t) -> t
  val form       : (Dom_html.formElement         Js.t -> 'a Network.t) -> t
  val frameset   : (Dom_html.frameSetElement     Js.t -> 'a Network.t) -> t
  val frame      : (Dom_html.frameElement        Js.t -> 'a Network.t) -> t
  val h1         : (Dom_html.headingElement      Js.t -> 'a Network.t) -> t
  val h2         : (Dom_html.headingElement      Js.t -> 'a Network.t) -> t
  val h3         : (Dom_html.headingElement      Js.t -> 'a Network.t) -> t
  val h4         : (Dom_html.headingElement      Js.t -> 'a Network.t) -> t
  val h5         : (Dom_html.headingElement      Js.t -> 'a Network.t) -> t
  val h6         : (Dom_html.headingElement      Js.t -> 'a Network.t) -> t
  val head       : (Dom_html.headElement         Js.t -> 'a Network.t) -> t
  val hr         : (Dom_html.hrElement           Js.t -> 'a Network.t) -> t
  val html       : (Dom_html.htmlElement         Js.t -> 'a Network.t) -> t
  val iframe     : (Dom_html.iFrameElement       Js.t -> 'a Network.t) -> t
  val img        : (Dom_html.imageElement        Js.t -> 'a Network.t) -> t
  val input      : (Dom_html.inputElement        Js.t -> 'a Network.t) -> t
  val ins        : (Dom_html.modElement          Js.t -> 'a Network.t) -> t
  val label      : (Dom_html.labelElement        Js.t -> 'a Network.t) -> t
  val legend     : (Dom_html.legendElement       Js.t -> 'a Network.t) -> t
  val li         : (Dom_html.liElement           Js.t -> 'a Network.t) -> t
  val link       : (Dom_html.linkElement         Js.t -> 'a Network.t) -> t
  val map        : (Dom_html.mapElement          Js.t -> 'a Network.t) -> t
  val meta       : (Dom_html.metaElement         Js.t -> 'a Network.t) -> t
  val _object    : (Dom_html.objectElement       Js.t -> 'a Network.t) -> t
  val ol         : (Dom_html.oListElement        Js.t -> 'a Network.t) -> t
  val optgroup   : (Dom_html.optGroupElement     Js.t -> 'a Network.t) -> t
  val option     : (Dom_html.optionElement       Js.t -> 'a Network.t) -> t
  val p          : (Dom_html.paramElement        Js.t -> 'a Network.t) -> t
  val param      : (Dom_html.paramElement        Js.t -> 'a Network.t) -> t
  val pre        : (Dom_html.preElement          Js.t -> 'a Network.t) -> t
  val q          : (Dom_html.quoteElement        Js.t -> 'a Network.t) -> t
  val script     : (Dom_html.scriptElement       Js.t -> 'a Network.t) -> t
  val select     : (Dom_html.selectElement       Js.t -> 'a Network.t) -> t
  val style      : (Dom_html.styleElement        Js.t -> 'a Network.t) -> t
  val table      : (Dom_html.tableElement        Js.t -> 'a Network.t) -> t
  val tbody      : (Dom_html.tableSectionElement Js.t -> 'a Network.t) -> t
  val td         : (Dom_html.tableCellElement    Js.t -> 'a Network.t) -> t
  val textarea   : (Dom_html.textAreaElement     Js.t -> 'a Network.t) -> t
  val tfoot      : (Dom_html.tableSectionElement Js.t -> 'a Network.t) -> t
  val th         : (Dom_html.tableCellElement    Js.t -> 'a Network.t) -> t
  val thead      : (Dom_html.tableSectionElement Js.t -> 'a Network.t) -> t
  val title      : (Dom_html.titleElement        Js.t -> 'a Network.t) -> t
  val tr         : (Dom_html.tableRowElement     Js.t -> 'a Network.t) -> t
  val ul         : (Dom_html.uListElement        Js.t -> 'a Network.t) -> t
  val audio      : (Dom_html.audioElement        Js.t -> 'a Network.t) -> t
  val video      : (Dom_html.videoElement        Js.t -> 'a Network.t) -> t
end

module E = Element
