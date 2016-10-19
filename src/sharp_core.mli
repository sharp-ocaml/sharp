open Sharp_category

type time = float

module type Behaviour_base_S = sig
  type ('a, 'b) t
  type 'a event = ('a option, 'a) t

  val time : (time, 'a) t

  val map : ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t
  val pure : 'a -> ('a, 'b) t
  val apply : ('a -> 'b, 'c) t -> ('a, 'd) t -> ('b, 'e) t
  val join : (('a, 'b) t, 'c) t -> ('a, 'd) t

  val contramap : ('a, 'b) t -> f:('c -> 'b) -> ('a, 'c) t

  val ( <$?> ) : ('a -> 'b) -> ('a option, 'c) t -> ('b option, 'c) t
  val ( <*?> ) :
    (('a -> 'b) option, 'c) t -> ('a option, 'd) t -> ('b option, 'e) t

  val event : unit -> ('a option, 'a) t
  val trigger : ('a, 'b) t -> 'b -> time option

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

  val start : 'a t -> unit -> unit
  val add_funnel : ((time -> unit) -> unit -> unit) -> unit t
  val add_sink : (time -> unit) -> unit t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val join : 'a t t -> 'a t

  val perform_state_post : ('a, 'b) Behaviour.t -> init:'c
                           -> f:('c -> 'a -> 'c * (unit -> unit)) -> unit t
  val perform_state : ('a, 'b) Behaviour.t -> init:'c -> f:('c -> 'a -> 'c)
                      -> unit t
  val perform : ('a, 'b) Behaviour.t -> f:('a -> unit) -> unit t
  val react : ('a option, 'b) Behaviour.t -> ('c, 'd) Behaviour.t
              -> f:('a -> 'c -> unit) -> unit t

  val initially : (unit -> unit) -> unit t
  val finally : (unit -> unit) -> unit t
end

module type Network_extra_S = sig
  type 'a t
  val event : (('a -> unit) -> (unit -> unit)) -> 'a Behaviour.event t
  val unbound_event : unit -> 'a Behaviour.event t
end

module Network : sig
  module Base : Network_base_S

  include Network_base_S
  include Monad.NoInfix with type ('a, 'b) t := ('a, 'b) secondary_t
  include Network_extra_S with type 'a t := 'a t

  module Infix : Monad.S with type ('a, 'b) t := ('a, 'b) secondary_t
end
