open Sharp_core

type route = string list -> (unit -> unit -> unit) option

val router : ?base_path:string -> route list
             -> string list Behaviour.event Network.t

module type Part = sig
  type t
  type parse_func
  type 'a generate_func

  val parse        : t -> parse_func -> route
  val generate     : t -> string list generate_func
  val generate_    : string list -> t -> string list generate_func
  val to_fragment  : t -> string generate_func
  val to_fragment_ : string -> t -> string generate_func
end

module Final : sig
  type t = Empty
  type parse_func       = unit -> unit -> unit
  type 'a generate_func = 'a

  include Part with type t := t and type parse_func       := parse_func
                                and type 'a generate_func := 'a generate_func

  val empty : t
end

module Var (Rest : Part) : sig
  type t = Var of Rest.t

  type parse_func       = string -> Rest.parse_func
  type 'a generate_func = string -> 'a Rest.generate_func

  include Part with type t := t and type parse_func       := parse_func
                                and type 'a generate_func := 'a generate_func

  val var : Rest.t -> t
end

module Const (Rest : Part) : sig
  type t = Const of string * Rest.t

  type parse_func       = Rest.parse_func
  type 'a generate_func = 'a Rest.generate_func

  include Part with type t := t and type parse_func       := parse_func
                                and type 'a generate_func := 'a generate_func

  val const : string -> Rest.t -> t
end

val empty : Final.t
val ( ^/ )  : ('a -> 'b) -> 'a -> 'b
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
