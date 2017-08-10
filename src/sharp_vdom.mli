(** An experimental implementation of a VDOM with helpers to link it to
    signals.

    Say [s1] is a signal of first name and last name, so its type is
    [(string * string) t], and [container] is a HTML element of type
    [Dom_html.element Js.t]. You can display a message in this element like
    this:

    {[
      let unlink =
        vdom container s1 (fun (first_name, last_name) ->
          tag "div" |* ("class", "message")
          |- text "Hello! You are "
          |- (tag "em" |- text (first_name ^ " " ^ last_name))
        )
      in
      (* Rest of your program. *)
      (* Calling unlink causes the VDOM to be destroyed. *)
    ]}
 *)

open Sharp_core

(** VDOM type. *)
type t

(** Run the function with the signal's value every time it "changes" value (see
    document of [Sharp.Core] for more details on that) and binds the returned
    VDOM to the given real DOM element. The returned callback can be called to
    stop reacting to this signal. *)
val vdom : Dom_html.element Js.t -> 'a Sharp_core.t -> ('a -> t)
           -> (unit -> unit)

(** When creating a node, a network can be given, the restart strategy
    determines when this network should be stopped and called again. *)
type restart_strategy =
  | Never | Always | OnChange | OnDeepChange | OnIdentifierChange of string

(** Create a VDOM node of the given HTML tag with the given list of children. *)
val node    : ?network:(Dom_html.element Js.t -> (unit -> unit))
              -> ?strategy:restart_strategy -> ?id:string -> string -> t list
              -> t
(** [network] is a function called when the actual node is created and whose
    returned function is called when the node is destroyed.
    [strategy] is the restart strategy for this node. See [restart_strategy].
    [id] allows you to identify a node in order to help the VDOM engine to
    distinguish between nodes.
 *)

(** Same as [node] but without children. *)
val element : ?network:(Dom_html.element Js.t -> (unit -> unit))
              -> ?strategy:restart_strategy-> ?id:string -> string -> t

(** Alias for [element]. *)
val tag     : ?network:(Dom_html.element Js.t -> (unit -> unit))
              -> ?strategy:restart_strategy -> ?id:string -> string -> t

(** A text node. *)
val text    : ?network:(Dom.text Js.t -> (unit -> unit))
              -> ?strategy:restart_strategy -> string -> t

(** Append a child to a node. The first parameter is the parent. *)
val append_child  : t -> t -> t

(** Prepend a child to a node. The first parameter is the parent. *)
val prepend_child : t -> t -> t

(** Set an attribute to a node. The first string is the attribute name and the
    second its value. *)
val set_attribute : string -> string -> t -> t

(** Remove an attribute from a node. *)
val clear_attribute : string -> t -> t

(** Alias for [append_child]. *)
val ( |- ) : t -> t -> t

(** Append several children at once. *)
val ( |+ ) : t -> t list -> t

(** Alias for [set_attribute] (sort of.) *)
val ( |* ) : t -> string * string -> t

module Linked : sig
  (** A linked VDOM, i.e. a VDOM value which corresponds to a real DOM node. *)
  type t
end

(** Link a VDOM to a node and return a pair with a linked VDOM and a callback to
    start the subnetworks.
    This is so that we can use perform_state_post to start the subnetworks after
    the new state has been recorded. *)
val link : ?current:Dom.node Js.t -> Dom_html.element Js.t -> t
           -> Linked.t * (unit -> unit)

val unlink : ?remove:bool -> Linked.t -> (unit -> unit)
val diff_and_patch : Dom_html.element Js.t -> Linked.t -> t
                     -> Linked.t * (unit -> unit)

(** Helpers for specific elements *)
module Element : sig
  val a          : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.anchorElement Js.t -> (unit -> unit)) -> t
  val area       : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.areaElement Js.t -> (unit -> unit)) -> t
  val article    : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.element Js.t -> (unit -> unit)) -> t
  val base       : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.baseElement Js.t -> (unit -> unit)) -> t
  val blockquote : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.quoteElement Js.t -> (unit -> unit)) -> t
  val body       : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.bodyElement Js.t -> (unit -> unit)) -> t
  val br         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.brElement Js.t -> (unit -> unit)) -> t
  val button     : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.buttonElement Js.t -> (unit -> unit)) -> t
  val canvas     : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.canvasElement Js.t -> (unit -> unit)) -> t
  val caption    : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableCaptionElement Js.t -> (unit -> unit)) -> t
  val col        : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableColElement Js.t -> (unit -> unit)) -> t
  val colgroup   : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableColElement Js.t -> (unit -> unit)) -> t
  val del        : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.modElement Js.t -> (unit -> unit)) -> t
  val div        : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.divElement Js.t -> (unit -> unit)) -> t
  val dl         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.dListElement Js.t -> (unit -> unit)) -> t
  val fieldset   : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.fieldSetElement Js.t -> (unit -> unit)) -> t
  val embed      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.embedElement Js.t -> (unit -> unit)) -> t
  val form       : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.formElement Js.t -> (unit -> unit)) -> t
  val frameset   : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.frameSetElement Js.t -> (unit -> unit)) -> t
  val frame      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.frameElement Js.t -> (unit -> unit)) -> t
  val h1         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.headingElement Js.t -> (unit -> unit)) -> t
  val h2         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.headingElement Js.t -> (unit -> unit)) -> t
  val h3         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.headingElement Js.t -> (unit -> unit)) -> t
  val h4         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.headingElement Js.t -> (unit -> unit)) -> t
  val h5         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.headingElement Js.t -> (unit -> unit)) -> t
  val h6         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.headingElement Js.t -> (unit -> unit)) -> t
  val header     : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.element Js.t -> (unit -> unit)) -> t
  val head       : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.headElement Js.t -> (unit -> unit)) -> t
  val hr         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.hrElement Js.t -> (unit -> unit)) -> t
  val html       : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.htmlElement Js.t -> (unit -> unit)) -> t
  val iframe     : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.iFrameElement Js.t -> (unit -> unit)) -> t
  val img        : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.imageElement Js.t -> (unit -> unit)) -> t
  val input      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.inputElement Js.t -> (unit -> unit)) -> t
  val ins        : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.modElement Js.t -> (unit -> unit)) -> t
  val label      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.labelElement Js.t -> (unit -> unit)) -> t
  val legend     : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.legendElement Js.t -> (unit -> unit)) -> t
  val li         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.liElement Js.t -> (unit -> unit)) -> t
  val link       : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.linkElement Js.t -> (unit -> unit)) -> t
  val map        : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.mapElement Js.t -> (unit -> unit)) -> t
  val meta       : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.metaElement Js.t -> (unit -> unit)) -> t
  val nav        : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.element Js.t -> (unit -> unit)) -> t
  val _object    : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.objectElement Js.t -> (unit -> unit)) -> t
  val ol         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.oListElement Js.t -> (unit -> unit)) -> t
  val optgroup   : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.optGroupElement Js.t -> (unit -> unit)) -> t
  val option     : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.optionElement Js.t -> (unit -> unit)) -> t
  val p          : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.paramElement Js.t -> (unit -> unit)) -> t
  val param      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.paramElement Js.t -> (unit -> unit)) -> t
  val pre        : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.preElement Js.t -> (unit -> unit)) -> t
  val q          : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.quoteElement Js.t -> (unit -> unit)) -> t
  val script     : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.scriptElement Js.t -> (unit -> unit)) -> t
  val section    : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.element Js.t -> (unit -> unit)) -> t
  val select     : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.selectElement Js.t -> (unit -> unit)) -> t
  val style      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.styleElement Js.t -> (unit -> unit)) -> t
  val table      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableElement Js.t -> (unit -> unit)) -> t
  val tbody      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableSectionElement Js.t -> (unit -> unit)) -> t
  val td         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableCellElement Js.t -> (unit -> unit)) -> t
  val textarea   : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.textAreaElement Js.t -> (unit -> unit)) -> t
  val tfoot      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableSectionElement Js.t -> (unit -> unit)) -> t
  val th         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableCellElement Js.t -> (unit -> unit)) -> t
  val thead      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableSectionElement Js.t -> (unit -> unit)) -> t
  val title      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.titleElement Js.t -> (unit -> unit)) -> t
  val tr         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.tableRowElement Js.t -> (unit -> unit)) -> t
  val ul         : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.uListElement Js.t -> (unit -> unit)) -> t
  val audio      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.audioElement Js.t -> (unit -> unit)) -> t
  val video      : ?strategy:restart_strategy -> ?id:string
                   -> (Dom_html.videoElement Js.t -> (unit -> unit)) -> t
end

module E = Element
