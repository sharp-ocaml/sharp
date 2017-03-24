type void = Void of void

module Functor = struct
  module type Base = sig
    type ('x, 'a) t
    val map : ('x, 'a) t -> f:('a -> 'b) -> ('x, 'b) t
  end

  module type NoInfix = sig
    include Base
    val void : ('x, 'a) t -> ('x, unit) t
    val replace : ('x, 'a) t -> 'b -> ('x, 'b) t
  end

  module type S = sig
    include NoInfix
    val ( <$> ) : ('a -> 'b) -> ('x, 'a) t -> ('x, 'b) t
    val ( <$  ) : 'a -> ('x, 'b) t -> ('x, 'a) t
    val (  $> ) : ('x, 'a) t -> 'b -> ('x, 'b) t
  end

  module MakeNoInfix (B : Base)
         : (NoInfix with type ('x, 'a) t := ('x, 'a) B.t) = struct
    include B
    let const x y = x
    let void fx = B.map ~f:(const ()) fx
    let replace fx y = B.map ~f:(const y) fx
  end

  module Make (B : Base) : (S with type ('x, 'a) t := ('x, 'a) B.t) = struct
    include MakeNoInfix(B)
    let ( <$> ) f fx = map ~f fx
    let ( <$  ) y fx = replace fx y
    let (  $> ) fx y = replace fx y
  end
end

module Applicative = struct
  module type Base = sig
    include Functor.Base
    val pure : 'a -> (void, 'a) t
    val apply : ('x, ('a -> 'b)) t -> ('y, 'a) t -> (void, 'b) t
  end

  module type NoInfix = sig
    include Base
    include Functor.NoInfix with type ('x, 'a) t := ('x, 'a) t

    val lift0 : 'a -> (void, 'a) t
    val lift  : ('a -> 'b) -> ('x, 'a) t -> ('x, 'b) t
    val lift2 : ('a -> 'b -> 'c) -> ('x0, 'a) t -> ('x1, 'b) t -> (void, 'c) t
    val lift3 : ('a -> 'b -> 'c -> 'd) -> ('x0, 'a) t -> ('x1, 'b) t
                -> ('x2, 'c) t -> (void, 'd) t
    val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ('x0, 'a) t -> ('x1, 'b) t
                -> ('x2, 'c) t -> ('x3, 'd) t -> (void, 'e) t
    val lift5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('x0, 'a) t -> ('x1, 'b) t
                -> ('x2, 'c) t -> ('x3, 'd) t -> ('x4, 'e) t -> (void, 'f) t

    val sequence : ('x, 'a) t list -> (void, 'a list) t
  end

  module type S = sig
    include NoInfix
    include Functor.S with type ('x, 'a) t := ('x, 'a) t

    val ( <*> ) : ('x, ('a -> 'b)) t -> ('y, 'a) t -> (void, 'b) t
    val ( <*  ) : ('x, 'a) t -> ('y, 'b) t -> (void, 'a) t
    val (  *> ) : ('x, 'a) t -> ('y, 'b) t -> (void, 'b) t
  end

  module MakeNoInfix (B : Base)
         : (NoInfix with type ('x, 'a) t := ('x, 'a) B.t) = struct
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

  module Make (B : Base) : (S with type ('x, 'a) t := ('x, 'a) B.t) = struct
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
    val join : ('x, ('y, 'a) t) t -> (void, 'a) t
  end

  module type NoInfix = sig
    include Base
    include Applicative.NoInfix with type ('x, 'a) t := ('x, 'a) t

    val return : 'a -> (void, 'a) t
    val bind : ('x, 'a) t -> f:('a -> ('y, 'b) t) -> (void, 'b) t
    val mapM : ('a -> ('x, 'b) t) -> 'a list -> (void, 'b list) t
  end

  module type S = sig
    include NoInfix
    include Applicative.S with type ('x, 'a) t := ('x, 'a) t

    val ( >>= ) : ('x, 'a) t -> ('a -> ('y, 'b) t) -> (void, 'b) t
    val ( >> ) : ('x, 'a) t -> ('y, 'b) t -> (void, 'b) t
  end

  module MakeNoInfix (B : Base)
         : (NoInfix with type ('x, 'a) t := ('x, 'a) B.t) = struct
    include B
    include Applicative.MakeNoInfix(B)

    let return = pure
    let bind mx ~f = join (map ~f mx)
    let mapM f xs = sequence (List.map f xs)
  end

  module Make (B : Base) : (S with type ('x, 'a) t := ('x, 'a) B.t) = struct
    include MakeNoInfix(B)
    include Applicative.Make(B)

    let ( >>= ) mx f = bind mx ~f
    let ( >> ) fx fy = fx >>= fun _ -> fy
  end
end
