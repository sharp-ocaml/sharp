open Sharp_category
open Sharp_core

type time_diff = time

type 'a command =
  | Once   of 'a * time_diff
  | Every  of 'a * time_diff
  | NTimes of 'a * time_diff * int

val tick_manager : 'a command list
                   -> 'a option t * ('a command -> unit) * (unit -> unit)

val every : time_diff -> 'a -> 'a option t * (unit -> unit)
val last_for : time_diff -> 'a option t -> 'a option t
