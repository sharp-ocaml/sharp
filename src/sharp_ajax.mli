open Sharp_core

open XmlHttpRequest

val frame_receiver : unit -> 'a generic_http_frame Behaviour.event Network.t
val success_receiver : unit -> 'a Behaviour.event Network.t
val diff_receiver :
  unit -> ('a Behaviour.event * 'a generic_http_frame Behaviour.event) Network.t

val plug_lwt : 'a Behaviour.event -> 'a Lwt.t -> unit

val plug_frame : 'a generic_http_frame Behaviour.event
                 -> 'a generic_http_frame Lwt.t -> unit
val plug_success : 'a Behaviour.event -> 'a generic_http_frame Lwt.t -> unit
val plug_diff : 'a Behaviour.event -> 'a generic_http_frame Behaviour.event
                -> 'a generic_http_frame Lwt.t -> unit
