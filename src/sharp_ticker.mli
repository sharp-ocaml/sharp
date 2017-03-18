open Sharp_category
open Sharp_core

type time_diff = time

type 'a command =
  | Once   of 'a * time_diff
  | Every  of 'a * time_diff
  | NTimes of 'a * time_diff * int

val tick_manager :
  'a command list -> ('a command, 'a option) Signal.t Network.t

val every : time_diff -> 'a -> (void, 'a option) Signal.t Network.t

val last_for : time_diff -> ('a, 'b option) Signal.t
               -> ('a, 'b option) Signal.t
