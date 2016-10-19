open Sharp_core

open XmlHttpRequest

exception HTTPFailure of http_frame

val frame_receiver : unit -> 'a generic_http_frame Behaviour.event Network.t
val success_receiver : unit -> 'a Behaviour.event Network.t
val result_receiver : unit -> (string, exn) result Behaviour.event Network.t
val diff_receiver :
  unit -> (string Behaviour.event * exn Behaviour.event) Network.t

val plug_lwt : ('a, 'b) Behaviour.t -> 'b Lwt.t -> unit
val plug_lwt_result : ('a, ('b, exn) result) Behaviour.t -> 'b Lwt.t -> unit

val plug_frame :
  ('a, 'b generic_http_frame) Behaviour.t -> 'b generic_http_frame Lwt.t -> unit
val plug_success : ('a, 'b) Behaviour.t -> 'b generic_http_frame Lwt.t -> unit
val plug_result :
  ('a, (string, exn) result) Behaviour.t -> http_frame Lwt.t -> unit
val plug_diff :
  ('a, string) Behaviour.t -> ('c, exn) Behaviour.t -> http_frame Lwt.t -> unit
