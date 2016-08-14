type time = float

module Behaviour :
  sig
    type 'a t
    val at : 'a t -> time -> 'a * 'a t
    val time : time t

    val lift0 : 'a -> 'a t
    val lift1 : f:('a -> 'b) -> 'a t -> 'b t
    val lift2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val lift3 : f:('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

    val const : 'a -> 'a t
    val map : f:('a -> 'b) -> 'a t -> 'b t

    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

    val ( <* ) : 'a t -> 'b t -> 'a t
    val ( *> ) : 'a t -> 'b t -> 'b t

    val event : unit -> 'a option t * (time -> 'a -> unit)

    val on : init:'a -> f:('a -> 'b -> 'a) -> 'b option t -> 'a t
    val last : init:'a -> 'a option t -> 'a t
    val toggle : init:bool -> 'a option t -> bool t
    val count : ?init:int -> 'a option t -> int t
  end

module Behavior = Behaviour

module Network :
  sig
    type 'a t
    val start : 'a t -> unit -> unit

    val add_funnel : ((time -> unit) -> (unit -> unit)) -> unit t
    val add_sink : (time -> unit) -> unit t

    val map : f:('a -> 'b) -> 'a t -> 'b t

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >> ) : 'a t -> 'b t -> 'b t

    val react : 'a Behaviour.t -> f:('a -> unit) -> unit t
  end
