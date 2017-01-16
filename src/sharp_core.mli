open Sharp_category

type time = float

module type Behaviour_base_S = sig
  type 'a behaviour = B of (time -> 'a * 'a behaviour)
  type 'a event_trigger = 'a -> time

  type ('a, 'b) t =
    { behaviour : 'a behaviour
    ; trigger   : 'b event_trigger option
    }

  type 'a event = ('a option, 'a) t

  val at : 'a behaviour -> time -> 'a * 'a behaviour

  val time : (time, 'a) t

  val no_trigger : ('a, 'b) t -> ('a, 'c) t

  val map : ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t
  val pure : 'a -> ('a, 'b) t
  val apply : ('a -> 'b, 'c) t -> ('a, 'd) t -> ('b, 'e) t
  val join : (('a, 'b) t, 'c) t -> ('a, 'd) t

  val contramap : ('a, 'b) t -> f:('c -> 'b) -> ('a, 'c) t
  val contramap_opt : ('a, 'b) t -> f:('c -> 'b option) -> ('a, 'c) t

  val ( <$?> ) : ('a -> 'b) -> ('a option, 'c) t -> ('b option, 'c) t
  val ( <*?> ) :
    (('a -> 'b) option, 'c) t -> ('a option, 'd) t -> ('b option, 'e) t

  val event : unit -> ('a option, 'a) t
  val trigger : ('a, 'b) t -> 'b -> time option

  val combine : ('a, 'b) t -> ('c, 'd) t -> ('a, 'd) t

  val on : ('a option, 'b) t -> init:'c -> f:('c -> 'a -> 'c) -> ('c, 'b) t
  val last : ('a option, 'b) t -> init:'a -> ('a, 'b) t
  val toggle : ('a option, 'b) t -> init:bool -> (bool, 'b) t
  val count : ?init:int -> ('a option, 'b) t -> (int, 'b) t
  val upon : ?init:'a -> ('b option, 'c) t -> ('a, 'd) t -> ('a, 'c) t

  val fold : ('a -> 'b -> 'a) -> 'a -> ('b, 'c) t -> ('a, 'd) t
end

module Behaviour : sig
  module Base : Behaviour_base_S

  include Behaviour_base_S
  include Monad.NoInfix with type ('a, 'b) t := ('a, 'b) t

  module Infix : Monad.S with type ('a, 'b) t := ('a, 'b) t
end

module Behavior = Behaviour

module type Network_base_S = sig
  type 'a t
  type ('a, 'b) secondary_t = 'a t
  type manager

  val noop_manager : manager

  val start : 'a t -> manager
  val flush : manager -> time -> unit
  val stop  : manager -> unit

  val add_funnel : ((time -> unit) -> unit -> unit) -> unit t
  val add_sink : (time -> unit) -> unit t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val join : 'a t t -> 'a t

  val perform_state_post : ?finally:('c -> unit) -> ('a, 'b) Behaviour.t
                           -> init:'c -> f:('c -> 'a -> 'c * (unit -> unit))
                           -> unit t
  val perform_state : ?finally:('c -> unit) -> ('a, 'b) Behaviour.t -> init:'c
                      -> f:('c -> 'a -> 'c) -> unit t
  val perform : ('a, 'b) Behaviour.t -> f:('a -> unit) -> unit t
  val react : ('a option, 'b) Behaviour.t -> ('c, 'd) Behaviour.t
              -> f:('a -> 'c -> unit) -> unit t
  val react_ : ('a option, 'b) Behaviour.t -> f:('a -> unit) -> unit t

  val initially : (unit -> unit) -> unit t
  val finally : (unit -> unit) -> unit t
end

module type Network_extra_S = sig
  type 'a t
  val event : ?connect:(('a -> unit) -> (unit -> unit))
              -> unit -> 'a Behaviour.event t
end

module Network : sig
  module Base : Network_base_S

  include Network_base_S
  include Monad.NoInfix with type ('a, 'b) t := ('a, 'b) secondary_t
  include Network_extra_S with type 'a t := 'a t

  module Infix : Monad.S with type ('a, 'b) t := ('a, 'b) secondary_t
end
