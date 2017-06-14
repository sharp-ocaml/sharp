type time = float

(* Avoid relying on this implementation as much as possible *)
type 'a t =
  { timed_value  : time -> 'a * 'a t
  ; propagateref : (time -> unit) ref
  }

val at : 'a t -> time -> 'a * 'a t

val const : 'a -> 'a t
val time : time t

val map : ('a -> 'b) -> 'a t -> 'b t
val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

val map_opt : ('a -> 'b) -> 'a option t -> 'b option t
val ( <$?> ) : ('a -> 'b) -> 'a option t -> 'b option t

val pure : 'a -> 'a t
val apply : ('a -> 'b) t -> 'a t -> 'b t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
val lift0 : 'a -> 'a t
val lift : ('a -> 'b) -> 'a t -> 'b t
val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
val lift5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t
            -> 'e t -> 'f t

val sequence : 'a t list -> 'a list t

val return : 'a -> 'a t
val join : 'a t t -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >> ) : 'a t -> 'b t -> 'b t

val perform : 'a t -> ('a -> unit) -> unit
val perform_state : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> unit
val perform_state_post :
  'a t -> init:'b -> f:('b -> 'a -> 'b * (unit -> unit)) -> unit

val react : 'a option t -> ('a -> unit) -> unit
val react_with : 'a option t -> 'b t -> ('a -> 'b -> unit) -> unit

val event : unit -> 'a option t * ('a -> unit)
val connected_event : (('a -> unit) -> (unit -> unit))
                      -> 'a option t * ('a -> unit) * (unit -> unit)

val on : 'a option t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
val last : 'a option t -> init:'a -> 'a t
val toggle : 'b option t -> init:bool -> bool t
val count : ?init:int -> 'b option t -> int t
val upon : ?init:'a -> 'c option t -> 'a t -> 'a t

val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
