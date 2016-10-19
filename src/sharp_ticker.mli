open Sharp_core

type time_diff = time

type 'a command =
  | Once   of 'a * time_diff
  | Every  of 'a * time_diff
  | NTimes of 'a * time_diff * int

val tick_manager :
  'a command list -> ('a option, 'a command) Behaviour.t Network.t

val every : time_diff -> 'a -> ('a option, 'b) Behaviour.t Network.t

val last_for : time_diff -> ('a option, 'b) Behaviour.t
               -> ('a option, 'b) Behaviour.t
