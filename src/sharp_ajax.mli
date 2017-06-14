open Sharp_core

open XmlHttpRequest

exception HTTPFailure of http_frame

val frame_receiver :
  unit -> 'a generic_http_frame option t * ('a generic_http_frame -> unit)
val success_receiver : unit -> 'a option t * ('a -> unit)
val result_receiver :
  unit -> (string, exn) result option t * ((string, exn) result -> unit)
val diff_receiver :
  unit -> (string option t * (string -> unit)) * (exn option t * (exn -> unit))

val plug_lwt : ('a -> unit) -> 'a Lwt.t -> unit
val plug_lwt_result : (('a, exn) result -> unit) -> 'a Lwt.t -> unit

val plug_frame   : ('a generic_http_frame -> unit)
                   -> 'a generic_http_frame Lwt.t -> unit
val plug_success : ('a -> unit) -> 'a generic_http_frame Lwt.t -> unit
val plug_result  : ((string, exn) result -> unit) -> http_frame Lwt.t -> unit
val plug_diff    : (string -> unit) -> (exn -> unit) -> http_frame Lwt.t -> unit
