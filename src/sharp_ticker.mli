(** Signals that takes value at certain times (such as every 5 minutes.) *)

open Sharp_core

type time_diff = time

(** A command that can be given to a tick manager. *)
type 'a command =
  | Once   of 'a * time_diff (** Take the value after x seconds. *)
  | Every  of 'a * time_diff (** Take the value every x seconds. *)
  | NTimes of 'a * time_diff * int (** Take the value every x seconds n times
                                       then stops. *)

(** Take a list of initial commands and return the signal, a trigger function to
    send new commands and a callback to stop the tick manager. *)
val tick_manager : 'a command list
                   -> 'a option t * ('a command -> unit) * (unit -> unit)

(** Create a signal which takes the given value every x seconds. *)
val every : time_diff -> 'a -> 'a option t * (unit -> unit)

(** Transform a signal so that it keeps the last value for x seconds even though
    the underlying signal does not have a value anynore. *)
val last_for : time_diff -> 'a option t -> 'a option t
(** Note that this does not bind any events. Hence, nothing is triggered
    automatically after these x seconds. This signal should therefore be bound
    to a ticker somehow before it is connected to a function like [vdom]. *)
