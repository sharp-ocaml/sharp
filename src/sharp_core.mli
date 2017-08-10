(**
   This the main module containing the code of the project. It defines how
   signals operate and how they can be used. Simply put, signals are
   time-varying value to which the code can react.

   More precisely, they are functions taking the time and returning a value of
   the appropriate type and a new signal which can be used with future times.

   Signals are functors, applicative functors and monads. They can be combined
   just like any other applicative functors or monads. For example,
   [map f s1] maps the function [f] on the values taken by the signal [s1]
   returning a signal of the results. [apply (map f s1) s2] or, more
   idiomatically, [f <$> s1 <*> s2] applies the function [f] on the values taken
   by the signals [s1] and [s2] and returns a signal of the results. Of course,
   [g <$> s1 <*> s2 <*> s3] will work with a function [g] taking three arguments
   (and so forth). Finally, you can bind two signals sequentially with the
   monadic interface. Say you have a signal [s1] returning the time and a
   function [f] taking the time as an argument and returning a signal of
   integers, you can bind both with [bind s1 f] or [s1 >>= f] to yield a signal
   of integers. The monadic bind on signals can be a bit tricky but [map] and
   [apply] should be more than often in most cases.

   Signals are useless if they are not connected to the real world somehow.

   To input data into a network of signals, you will need the function [event].
   It returns a optional signal (i.e. a signal of type ['a option t], that is, a
   signal taking values of type ['a option] together with a function to trigger
   the event. Whenever this trigger is called with a value [x], the signal will
   take the value [Some x], otherwise it will be [None].

   In order to react to events, there are a few functions which roughly work the
   same way. The ones you will use the most are [react_with] and [react] which
   is a specialised version of [react_with]. [react_with] reacts to a signal of
   type ['a option t] when it has a value and another "normal signal" and
   executes a given function with the values taken by the signals. Here is a
   simple example:

   {[
        let (int_signal, trigger_int) = event () in
        react_with int_signal time (fun x t ->
            print_endline ("The time is " ^ string_of_float(t)
                           ^ "and the event has been triggered with the value: "
                           ^ string_of_int(x))
          )
   ]}

   This module also includes a couple of functions transforming signals. Other
   modules provide ways of binding signal networks to the external world, e.g.
   reacting to the click of a button.
 *)

type time = float

(** Type of signals. *)
type 'a t

(**/**)
val at : 'a t -> time -> 'a * 'a t
val subscribe : 'a t -> ((unit -> unit) -> (time -> (unit -> exn option)))
                -> unit
val lock : 'a t -> unit -> (unit -> unit)

val make : (time -> 'a * 'a t)
           -> (((unit -> unit) -> (time -> (unit -> exn option))) -> unit)
           -> (unit -> (unit -> unit))
           -> 'a t
(**/**)

(** The simplest signal, always returning the same value. Note that, for
    convenience and to reflect the intent of your code more accurately, you
    can also use its aliases: return, pure and lift0. *)
val const : 'a -> 'a t

(** Return the current time. *)
val time : time t

(** Map a function onto a signal's (current and future) values. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Alias for [map]. *)
val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

(** Alias for [const]. *)
val pure : 'a -> 'a t

(** Signals are applicative functors. *)
val apply : ('a -> 'b) t -> 'a t -> 'b t

(** Alias for [apply]. *)
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

(** The following functions lift a function of values into a function of
    signals. *)
val lift0 : 'a -> 'a t
val lift : ('a -> 'b) -> 'a t -> 'b t
val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
val lift5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t
            -> 'e t -> 'f t

(** A kind of [map] on optional signals. *)
val map_opt : ('a -> 'b) -> 'a option t -> 'b option t

(** Alias for [map_opt]. *)
val ( <$?> ) : ('a -> 'b) -> 'a option t -> 'b option t

(** A kind [apply] on optional signals. *)
val apply_opt : ('a -> 'b) option t -> 'a option t -> 'b option t

(** Alias for [apply_opt]. *)
val ( <*?> ) : ('a -> 'b) option t -> 'a option t -> 'b option t

(** Combine a list of signals into a signal of list. *)
val sequence : 'a t list -> 'a list t

(** Alias for [const]. *)
val return : 'a -> 'a t

(** Flatten a nested signal. When called, the next signal will be determined by
    the outer signal and the new one determined by the inner one will be
    discarded. *)
val join : 'a t t -> 'a t

(** Monadic bind. It allows to sequentially compose signals together. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** Alias for [bind]. *)
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

(** Like [>>=] but it discards the value of the first signal. *)
val ( >> ) : 'a t -> 'b t -> 'b t

(** Perform an action every time the signal changes value. *)
val perform : ?force:bool -> 'a t -> ('a -> unit) -> unit
(**
   To be precise, the signal could have the same value as before but something
   happened upstream. If some signals have been triggered and the signal passed
   to [perform] somehow depends on it, through calls to [map] or [apply] for
   instance, the function given to [perform] will be called.
 *)

(** Like perform but maintain a state between calls. *)
val perform_state : ?force:bool -> 'a t -> init:'b -> f:('b -> 'a -> 'b) -> unit

(**/**)
val perform_state_post :
  ?force:bool -> 'a t -> init:'b -> f:('b -> 'a -> 'b * (unit -> unit)) -> unit
(**/**)

(** Run the function when the optional signal takes a value. *)
val react : 'a option t -> ('a -> unit) -> unit

(** Same as [react] with an additional signal. *)
val react_with : 'a option t -> 'b t -> ('a -> 'b -> unit) -> unit
(** This is useful for things like
    [react_with button_cliked data_signal (fun () data -> (* do something *))].
 *)

(** Create a optional signal with the function to give the signal the given
    value at the current time. *)
val event : unit -> 'a option t * ('a -> unit)

(** Create a signal from a callback and returns the signal, the trigger function
    and another callback to disconnect the event. *)
val connected_event : (('a -> unit) -> (unit -> unit))
                      -> 'a option t * ('a -> unit) * (unit -> unit)
(** The first callback takes the trigger function and should return the second
    callback to disconnect the event. To make its use clearer, imagine you want
    to create a signal which should be triggered automatically when some JS
    event occurs. The first callback would be a function starting to listen to
    a JS event, calling the trigger function when the event occurs and returning
    a callback to destroy the listener when it's called.
 *)

(** Return a signal starting with the value [init] and changing this value based
    on the previous one and a value of an optional signal when it takes one. *)
val on : 'a option t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

(** Return the last value taken by an optional signal, [init] if none. *)
val last : 'a option t -> init:'a -> 'a t

(** Toggle between [true] and [false] each time the given optional signal takes
    a value. *)
val toggle : 'b option t -> init:bool -> bool t

(** Count the number of times the optional signal has had a value. *)
val count : ?init:int -> 'b option t -> int t

(** The new signal will keep the same value until the optional value takes a
    value. *)
val upon : ?init:'a -> 'c option t -> 'a t -> 'a t

(** Fold the values taken by a signal. *)
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
(** It is important to understand that signals are only called when something
    happened. It is therefore not continuous. *)
