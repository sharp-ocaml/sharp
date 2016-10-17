module Functor = struct
  module type Base = sig
    type 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module type NoInfix = sig
    include Base
    val void : 'a t -> unit t
    val replace : 'a t -> 'b -> 'b t
  end

  module type S = sig
    include NoInfix
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <$  ) : 'a -> 'b t -> 'a t
    val (  $> ) : 'a t -> 'b -> 'b t
  end

  module MakeNoInfix (B : Base) : (NoInfix with type 'a t := 'a B.t) = struct
    include B
    let const x y = x
    let void fx = B.map ~f:(const ()) fx
    let replace fx y = B.map ~f:(const y) fx
  end

  module Make (B : Base) : (S with type 'a t := 'a B.t) = struct
    include MakeNoInfix(B)
    let ( <$> ) f fx = map ~f fx
    let ( <$  ) y fx = replace fx y
    let (  $> ) fx y = replace fx y
  end
end

module Applicative = struct
  module type Base = sig
    include Functor.Base
    val pure : 'a -> 'a t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
  end

  module type NoInfix = sig
    include Base
    include Functor.NoInfix with type 'a t := 'a t

    val lift0 : 'a -> 'a t
    val lift  : ('a -> 'b) -> 'a t -> 'b t
    val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
    val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t
                -> 'e t
    val lift5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t
                -> 'd t -> 'e t -> 'f t

    val sequence : 'a t list -> 'a list t
  end

  module type S = sig
    include NoInfix
    include Functor.S with type 'a t := 'a t

    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( <*  ) : 'a t -> 'b t -> 'a t
    val (  *> ) : 'a t -> 'b t -> 'b t
  end

  module MakeNoInfix (B : Base) : (NoInfix with type 'a t := 'a B.t) = struct
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

  module Make (B : Base) : (S with type 'a t := 'a B.t) = struct
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
    val join : 'a t t -> 'a t
  end

  module type NoInfix = sig
    include Base
    include Applicative.NoInfix with type 'a t := 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  end

  module type S = sig
    include NoInfix
    include Applicative.S with type 'a t := 'a t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >> ) : 'a t -> 'b t -> 'b t
  end

  module MakeNoInfix (B : Base) : (NoInfix with type 'a t := 'a B.t) = struct
    include B
    include Applicative.MakeNoInfix(B)

    let return = pure
    let bind mx ~f = join (map ~f mx)
    let mapM f xs = sequence (List.map f xs)
  end

  module Make (B : Base) : (S with type 'a t := 'a B.t) = struct
    include MakeNoInfix(B)
    include Applicative.Make(B)

    let ( >>= ) mx f = bind mx ~f
    let ( >> ) fx fy = fx >>= fun _ -> fy
  end
end
