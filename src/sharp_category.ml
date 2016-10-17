module Functor = struct
  module type Base = sig
    type ('a, 'x) t
    val map : ('a, 'x) t -> f:('a -> 'b) -> ('b, 'x) t
  end

  module type NoInfix = sig
    include Base
    val void : ('a, 'x) t -> (unit, 'x) t
    val replace : ('a, 'x) t -> 'b -> ('b, 'x) t
  end

  module type S = sig
    include NoInfix
    val ( <$> ) : ('a -> 'b) -> ('a, 'x) t -> ('b, 'x) t
    val ( <$  ) : 'a -> ('b, 'x) t -> ('a, 'x) t
    val (  $> ) : ('a, 'x) t -> 'b -> ('b, 'x) t
  end

  module MakeNoInfix (B : Base)
         : (NoInfix with type ('a, 'x) t := ('a, 'x) B.t) = struct
    include B
    let const x y = x
    let void fx = B.map ~f:(const ()) fx
    let replace fx y = B.map ~f:(const y) fx
  end

  module Make (B : Base) : (S with type ('a, 'x) t := ('a, 'x) B.t) = struct
    include MakeNoInfix(B)
    let ( <$> ) f fx = map ~f fx
    let ( <$  ) y fx = replace fx y
    let (  $> ) fx y = replace fx y
  end
end

module Applicative = struct
  module type Base = sig
    include Functor.Base
    val pure : 'a -> ('a, 'x) t
    val apply : (('a -> 'b), 'x) t -> ('a, 'y) t -> ('b, 'z) t
  end

  module type NoInfix = sig
    include Base
    include Functor.NoInfix with type ('a, 'x) t := ('a, 'x) t

    val lift0 : 'a -> ('a, 'x) t
    val lift  : ('a -> 'b) -> ('a, 'x) t -> ('b, 'x)t
    val lift2 : ('a -> 'b -> 'c) -> ('a, 'x0) t -> ('b, 'x1) t -> ('c, 'x2) t
    val lift3 : ('a -> 'b -> 'c -> 'd) -> ('a, 'x0) t -> ('b, 'x1) t
                -> ('c, 'x2) t -> ('d, 'x3) t
    val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a, 'x0) t -> ('b, 'x1) t
                -> ('c, 'x2) t -> ('d, 'x3) t -> ('e, 'x4) t
    val lift5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('a, 'x0) t -> ('b, 'x1) t
                -> ('c, 'x2) t -> ('d, 'x3) t -> ('e, 'x4) t -> ('f, 'x5) t

    val sequence : ('a, 'x) t list -> ('a list, 'y) t
  end

  module type S = sig
    include NoInfix
    include Functor.S with type ('a, 'x) t := ('a, 'x) t

    val ( <*> ) : (('a -> 'b), 'x) t -> ('a, 'y) t -> ('b, 'z) t
    val ( <*  ) : ('a, 'x) t -> ('b, 'y) t -> ('a, 'z) t
    val (  *> ) : ('a, 'x) t -> ('b, 'y) t -> ('b, 'z) t
  end

  module MakeNoInfix (B : Base)
         : (NoInfix with type ('a, 'x) t := ('a, 'x) B.t) = struct
    include B
    include Functor.MakeNoInfix(B)

    let lift0 = pure
    let lift  f fa = map ~f fa
    let lift2 f fa fb = apply (map ~f fa) fb
    let lift3 f fa fb fc = apply (apply (map ~f fa) fb) fc
    let lift4 f fa fb fc fd = apply (apply (apply (map ~f fa) fb) fc) fd
    let lift5 f fa fb fc fd fe =
      apply (apply (apply (apply (map ~f fa) fb) fc) fd) fe

    let rec sequence = function
      | [] -> pure []
      | fx :: fxs -> lift2 (fun x xs -> x :: xs) fx (sequence fxs)
  end

  module Make (B : Base) : (S with type ('a, 'x) t := ('a, 'x) B.t) = struct
    include MakeNoInfix(B)
    include Functor.Make(B)

    let ( <*> ) = apply
    let ( <*  ) fx fy = (fun x _ -> x) <$> fx <*> fy
    let (  *> ) fx fy = (fun _ y -> y) <$> fx <*> fy
  end
end

module Monad = struct
  module type Base = sig
    include Applicative.Base
    val join : (('a, 'x) t, 'y) t -> ('a, 'z) t
  end

  module type NoInfix = sig
    include Base
    include Applicative.NoInfix with type ('a, 'x) t := ('a, 'x) t

    val return : 'a -> ('a, 'x) t
    val bind : ('a, 'x) t -> f:('a -> ('b, 'y) t) -> ('b, 'z) t
    val mapM : ('a -> ('b, 'x) t) -> 'a list -> ('b list, 'y) t
  end

  module type S = sig
    include NoInfix
    include Applicative.S with type ('a, 'x) t := ('a, 'x) t

    val ( >>= ) : ('a, 'x) t -> ('a -> ('b, 'y) t) -> ('b, 'z) t
    val ( >> ) : ('a, 'x) t -> ('b, 'y) t -> ('b, 'z) t
  end

  module MakeNoInfix (B : Base)
         : (NoInfix with type ('a, 'x) t := ('a, 'x) B.t) = struct
    include B
    include Applicative.MakeNoInfix(B)

    let return = pure
    let bind mx ~f = join (map ~f mx)
    let mapM f xs = sequence (List.map f xs)
  end

  module Make (B : Base) : (S with type ('a, 'x) t := ('a, 'x) B.t) = struct
    include MakeNoInfix(B)
    include Applicative.Make(B)

    let ( >>= ) mx f = bind mx ~f
    let ( >> ) fx fy = fx >>= fun _ -> fy
  end
end
