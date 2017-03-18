open Sharp_category

type time = float

module type Signal_base_S = sig
  type 'a timed_value   = TV of (time -> 'a * 'a timed_value)
  type 'a event_trigger = 'a -> time option

  type ('a, 'b) t =
    { trigger     : 'a event_trigger
    ; timed_value : 'b timed_value
    }

  type 'a event = ('a, 'a option) t

  val at : ('a, 'b) t  -> time -> 'b * ('a, 'b) t

  val time : (void, time) t

  val no_trigger : ('a, 'b) t -> (void, 'b) t

  val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
  val pure : 'a -> (void, 'a) t
  val apply : ('a, 'b -> 'c) t -> ('d, 'b) t -> (void, 'c) t
  val join : ('a, ('b, 'c) t) t -> (void, 'c) t

  val contramap : ('a, 'b) t -> f:('c -> 'a) -> ('c, 'b) t
  val contramap_opt : ('a, 'b) t -> f:('c -> 'a option) -> ('c, 'b) t

  val ( <$?> ) : ('a -> 'b) -> ('c, 'a option) t -> ('c, 'b option) t
  val ( <*?> ) :
    ('a, ('b -> 'c) option) t -> ('d, 'b option) t -> (void, 'c option) t

  val event : unit -> 'a event
  val trigger : ('a, 'b) t -> 'a -> unit
  val trigger' : ('a, 'b) t -> 'a -> time option

  val combine : ('a, 'b) t -> ('c, 'd) t -> ('a, 'd) t

  val on : ('a, 'b option) t -> init:'c -> f:('c -> 'b -> 'c) -> ('a, 'c) t
  val last : ('a, 'b option) t -> init:'b -> ('a, 'b) t
  val toggle : ('a, 'b option) t -> init:bool -> ('a, bool) t
  val count : ?init:int -> ('a, 'b option) t -> ('a, int) t
  val upon : ?init:'a -> ('b, 'c option) t -> ('d, 'a) t -> (void, 'a) t

  val fold : ('c, 'b) t -> f:('a -> 'b -> 'a) -> init:'a -> ('c, 'a) t
end

module Signal : sig
  module Base : Signal_base_S

  include Signal_base_S
  include Monad.NoInfix with type ('a, 'b) t := ('a, 'b) t

  module Infix : Monad.S with type ('a, 'b) t := ('a, 'b) t
end

module type Network_base_S = sig
  type 'a t
  type ('x, 'a) secondary_t = 'a t
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

  val perform_state_post : ?finally:('a -> unit) -> ('b, 'c) Signal.t
                           -> init:'a -> f:('a -> 'c -> 'a * (unit -> unit))
                           -> unit t
  val perform_state : ?finally:('a -> unit) -> ('b, 'c) Signal.t -> init:'a
                      -> f:('a -> 'c -> 'a) -> unit t
  val perform : ('a, 'b) Signal.t -> f:('b -> unit) -> unit t
  val react : ('a, 'b option) Signal.t -> ('c, 'd) Signal.t
              -> f:('b -> 'd -> unit) -> unit t
  val react_ : ('a, 'b option) Signal.t -> f:('b -> unit) -> unit t

  val initially : (unit -> unit) -> unit t
  val finally : (unit -> unit) -> unit t
end

module type Network_extra_S = sig
  type 'a t
  val event : ?connect:(('a -> unit) -> (unit -> unit))
              -> unit -> 'a Signal.event t
end

module Network : sig
  module Base : Network_base_S

  include Network_base_S
  include Monad.NoInfix with type ('a, 'b) t := ('a, 'b) secondary_t
  include Network_extra_S with type 'a t := 'a t

  module Infix : Monad.S with type ('a, 'b) t := ('a, 'b) secondary_t
end
