open Sharp_category

type time = float

module type Behaviour_base_S = sig
  type 'a t
  type 'a event_callback = time -> 'a -> unit
  type 'a event = 'a option t * 'a event_callback

  val at : 'a t -> time -> 'a * 'a t
  val time : time t

  val at : 'a t -> time -> 'a * 'a t
  val time : time t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val join : 'a t t -> 'a t

  val ( <$?> ) : ('a -> 'b) -> 'a option t -> 'b option t
  val ( <*?> ) : ('a -> 'b) option t -> 'a option t -> 'b option t
  val ( <|> ) : 'a option t -> 'a option t -> 'a option t

  val event : unit -> 'a event
  val to_behaviour : 'a event -> 'a option t
  val to_behavior : 'a event -> 'a option t
  val trigger : 'a event -> time -> 'a -> unit

  val on : 'a event -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
  val last : 'a event -> init:'a -> 'a t
  val toggle : 'a event -> init:bool -> bool t
  val count : ?init:int -> 'a event -> int t
  val upon : ?init:'a -> 'b event -> 'a t -> 'a t
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
end

module Behaviour : sig
  module Base : Behaviour_base_S

  include Behaviour_base_S
  include Monad.NoInfix with type 'a t := 'a t

  module Infix : Monad.S with type 'a t := 'a t
end

module Behavior = Behaviour

module type Network_base_S = sig
  type 'a t
  val start : 'a t -> unit -> unit
  val add_funnel : ((time -> unit) -> unit -> unit) -> unit t
  val add_sink : (time -> unit) -> unit t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val join : 'a t t -> 'a t
  val react : 'a Behaviour.t -> init:'b -> f:('b -> 'a -> 'b) -> unit t
  val react_ : 'a Behaviour.t -> f:('a -> 'b) -> unit t
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
  include Monad.NoInfix with type 'a t := 'a t
  include Network_extra_S with type 'a t := 'a t

  module Infix : Monad.S with type 'a t := 'a t
end
