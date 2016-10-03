open Sharp_core

open XmlHttpRequest

val ajax_receiver :
  unit -> ('a option Behaviour.t * 'a Behaviour.event_callback) Network.t

val plug_lwt : 'a Behaviour.event_callback -> 'a Lwt.t -> unit

val plug_ajax_200 :
  'a Behaviour.event_callback -> 'a generic_http_frame Lwt.t -> unit
