open TypeClasses

module Identity = struct
  module Base = struct
    type 'a t = 'a
    let fmap f a = f a
  end

  module IdentityFunctor = struct
    type 'a t = 'a
    include Functor.Make (Base)
    let pure a = a
    let apply f a = f a
  end

  module IdentityApplicative = struct
    type 'a t = 'a
    include Applicative.Make (IdentityFunctor)
    let bind a f = f a
  end

  include Base
  include Monad.Make (IdentityApplicative)
end

module Fun = struct
  module Base = struct
    type ('a, 'b) t = 'a -> 'b
    let fmap f g a = f (g a)
  end

  module FunFunctor = struct
    type ('a, 'b) t = 'a -> 'b
    include Functor.Make2 (Base)
    let pure = Fun.const
    let apply f g x = f x (g x)
  end

  module FunApplicative = struct
    include Applicative.Make2 (FunFunctor)
    let bind g f x = f (g x) x
  end

  include Base
  include Monad.Make2 (FunApplicative)
  include Fun
end

module Option = struct
  module Base = struct
    type 'a t = 'a option
    let fmap = Option.map
  end

  module OptionFunctor = struct
    type 'a t = 'a option
    include Functor.Make (Base)
    let pure a = Some a
    let apply af a =
      match af, a with
      | Some f, Some x -> Some (f x)
      | _ -> None
  end

  module OptionApplicative = struct
    type 'a t = 'a option
    include Applicative.Make (OptionFunctor)
    let bind = Option.bind
  end

  include Monad.Make (OptionApplicative)
  include Option
end

module Result = struct
  module Base = struct
    type ('e, 'a) t = ('a, 'e) result
    let fmap = Result.map
  end

  module ResultFunctor = struct
    type ('e, 'a) t = ('a, 'e) result
    include Functor.Make2 (Base)

    let pure a = Ok a
    let apply af a =
      match af, a with
      | Ok f, Ok x -> Ok (f x)
      | Error e, _ | Ok _, Error e -> Error e
  end

  module ResultApplicative = struct
    include Applicative.Make2 (ResultFunctor)
    let bind = Result.bind
  end

  include Result
  include Base
  include Monad.Make2 (ResultApplicative)
end

module List = struct
  module Base = struct
    type 'a t = 'a list
    let fmap = List.map
  end

  module ListFunctor = struct
    type 'a t = 'a list
    include Functor.Make (Base)
    let pure a = [a]
    let apply af a =
      List.concat @@ fmap (fun f ->
        fmap (fun x ->
          f x) a) af
  end

  module ListApplicative = struct
    type 'a t = 'a list
    include Applicative.Make (ListFunctor)
    let bind a f = List.concat @@ fmap f a
  end

  include Monad.Make (ListApplicative)
  include List
end
