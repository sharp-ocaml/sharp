(** A router which reacts to change to the page hash.

    The creation of routes is a bit awkward due to a lack of higher-kinded
    polymorphism in Ocaml. Paths are either constant strings or variables
    separated by /'s. It's easier to show how to create routes than trying
    to explain them directly so here are some examples: (C stands for constant,
    V for variable and F for final)

    {[
      (* open Sharp.Router *)

      let network () =
        (* Remember vdom returns a callback to stop itself. *)
        vdom container time (fun t -> text ("The time is " ^ string_of_float t))
      in
      let route1 = empty (* / *)
      and route2 = CF.const "hello" empty (* /hello *)
      and route3 =
        CVCF.const "users" (VCF.var (CF.const "hello" empty))
        (* /users/:some_value/hello *)
      and route4 =
        CVCF.const "users" ^/ VCF.var ^// CF.const "hello"
        (* Same as route3 but with convenient operators. You can use ^/ to avoid
           parentheses everywhere and the latest should be ^// instead. *)
      in
      let routes =
        [ Final.parse route1 network
        ; CF.parse route2 network
        ; CVCF.parse route3 (fun path_value () -> network ())
            (* Here we discard the string matched in the path. *)
        ]
      in
      router_ routes (* This call returns a callback to stop. *)
    ]}

    Links can now point to, say, [#/hello]. The router will stop the current
    network and start the new one. It will also replaces the path from
    [/whatever/it/was#/hello] to [/hello]. When started the router also looks at
    the current hash and path (in that order of priority) and checks if it needs
    to start some network. For example, if the path is [/hello] when the router
    is started (typically when the page is loaded), it will start the
    corresponding network. If it is [/hello#/], it will start the network
    corresponding to [/], not [/hello] and it will replace the path by [/].
 *)

open Sharp_core

(** A route that can be handled by the router. *)
type 'a route = string list -> ('a -> (unit -> unit)) option
(** You probably want to create them using the modules provided here. *)

(** Start listening to changes to the hash. *)
val router : ?base_path:string -> 'a t -> 'a route list -> (unit -> unit)
(** When a route matches, the current network is stopped and the network
    returned by the router is started. *)

(** Same as [router] but without an additional signal value. *)
val router_ : ?base_path:string -> unit route list -> (unit -> unit)

module type Part = sig
  type t
  type 'a parse_func
  type 'a parse_opt_func
  type 'a generate_func

  val parse        : t -> 'a parse_func -> 'a route
  val parse_opt    : t -> 'a parse_opt_func -> 'a route
  val generate     : t -> string list generate_func
  val generate_    : string list -> t -> string list generate_func
  val to_fragment  : t -> string generate_func
  val to_fragment_ : string -> t -> string generate_func
end

module Final : sig
  type t = Empty

  type 'a parse_func     = 'a -> (unit -> unit)
  type 'a parse_opt_func = ('a -> (unit -> unit)) option
  type 'a generate_func  = 'a

  include Part with type t := t and type 'a parse_func     := 'a parse_func
                                and type 'a parse_opt_func := 'a parse_opt_func
                                and type 'a generate_func  := 'a generate_func

  val empty : t
end

module Var (Rest : Part) : sig
  type t = Var of Rest.t

  type 'a parse_func     = string -> 'a Rest.parse_func
  type 'a parse_opt_func = string -> 'a Rest.parse_opt_func
  type 'a generate_func  = string -> 'a Rest.generate_func

  include Part with type t := t and type 'a parse_func     := 'a parse_func
                                and type 'a parse_opt_func := 'a parse_opt_func
                                and type 'a generate_func  := 'a generate_func

  val var : Rest.t -> t
end

module Const (Rest : Part) : sig
  type t = Const of string * Rest.t

  type 'a parse_func     = 'a Rest.parse_func
  type 'a parse_opt_func = 'a Rest.parse_opt_func
  type 'a generate_func  = 'a Rest.generate_func

  include Part with type t := t and type 'a parse_func     := 'a parse_func
                                and type 'a parse_opt_func := 'a parse_opt_func
                                and type 'a generate_func  := 'a generate_func

  val const : string -> Rest.t -> t
end

(** Same as [Final.empty]. Match the path [/]. *)
val empty : Final.t

(** See top documentation. *)
val ( ^/ )  : ('a -> 'b) -> 'a -> 'b

(** See top documentation. *)
val ( ^// ) : ('a -> 'b) -> (Final.t -> 'a) -> 'b

module CF : module type of Const(Final)
module VF : module type of Var(Final)

module CCF : module type of Const(CF)
module VCF : module type of Var(CF)
module VVF : module type of Var(VF)
module CVF : module type of Const(VF)

module CCCF : module type of Const(CCF)
module CVCF : module type of Const(VCF)
module CVVF : module type of Const(VVF)
module CCVF : module type of Const(CVF)
module VCCF : module type of Var(CCF)
module VVCF : module type of Var(VCF)
module VVVF : module type of Var(VVF)
module VCVF : module type of Var(CVF)

module CCCCF : module type of Const(CCCF)
module CCVCF : module type of Const(CVCF)
module CCVVF : module type of Const(CVVF)
module CCCVF : module type of Const(CCVF)
module CVCCF : module type of Const(VCCF)
module CVVCF : module type of Const(VVCF)
module CVVVF : module type of Const(VVVF)
module CVCVF : module type of Const(VCVF)
module VCCCF : module type of Var(CCCF)
module VCVCF : module type of Var(CVCF)
module VCVVF : module type of Var(CVVF)
module VCCVF : module type of Var(CCVF)
module VVCCF : module type of Var(VCCF)
module VVVCF : module type of Var(VVCF)
module VVVVF : module type of Var(VVVF)
module VVCVF : module type of Var(VCVF)

module CCCCCF : module type of Const(CCCCF)
module CCCVCF : module type of Const(CCVCF)
module CCCVVF : module type of Const(CCVVF)
module CCCCVF : module type of Const(CCCVF)
module CCVCCF : module type of Const(CVCCF)
module CCVVCF : module type of Const(CVVCF)
module CCVVVF : module type of Const(CVVVF)
module CCVCVF : module type of Const(CVCVF)
module CVCCCF : module type of Const(VCCCF)
module CVCVCF : module type of Const(VCVCF)
module CVCVVF : module type of Const(VCVVF)
module CVCCVF : module type of Const(VCCVF)
module CVVCCF : module type of Const(VVCCF)
module CVVVCF : module type of Const(VVVCF)
module CVVVVF : module type of Const(VVVVF)
module CVVCVF : module type of Const(VVCVF)
module VCCCCF : module type of Var(CCCCF)
module VCCVCF : module type of Var(CCVCF)
module VCCVVF : module type of Var(CCVVF)
module VCCCVF : module type of Var(CCCVF)
module VCVCCF : module type of Var(CVCCF)
module VCVVCF : module type of Var(CVVCF)
module VCVVVF : module type of Var(CVVVF)
module VCVCVF : module type of Var(CVCVF)
module VVCCCF : module type of Var(VCCCF)
module VVCVCF : module type of Var(VCVCF)
module VVCVVF : module type of Var(VCVVF)
module VVCCVF : module type of Var(VCCVF)
module VVVCCF : module type of Var(VVCCF)
module VVVVCF : module type of Var(VVVCF)
module VVVVVF : module type of Var(VVVVF)
module VVVCVF : module type of Var(VVCVF)
