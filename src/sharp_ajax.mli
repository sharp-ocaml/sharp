open Sharp_core

open XmlHttpRequest

exception HTTPFailure of http_frame

val frame_receiver : unit -> 'a generic_http_frame Signal.event Network.t
val success_receiver : unit -> 'a Signal.event Network.t
val result_receiver : unit -> (string, exn) result Signal.event Network.t
val diff_receiver : unit -> (string Signal.event * exn Signal.event) Network.t

val plug_lwt : ('a, 'b) Signal.t -> 'a Lwt.t -> unit
val plug_lwt_result : (('a, exn) result, 'b) Signal.t -> 'a Lwt.t -> unit

val plug_frame :
  ('a generic_http_frame, 'b) Signal.t -> 'a generic_http_frame Lwt.t -> unit
val plug_success : ('a, 'b) Signal.t -> 'a generic_http_frame Lwt.t -> unit
val plug_result :
  ((string, exn) result, 'a) Signal.t -> http_frame Lwt.t -> unit
val plug_diff :
  (string, 'a) Signal.t -> (exn, 'c) Signal.t -> http_frame Lwt.t -> unit
