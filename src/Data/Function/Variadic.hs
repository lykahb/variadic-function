module Data.Function.Variadic
  ( -- * Decomposition and creation of functions
    Function (..),
    ConstructFunction,
    DeconstructFunction,

    -- * Helper classes for argument constraints
    EmptyConstraint,
    type (&),
  )
where

import Data.Kind (Type)

-- | Toolkit for creating and transforming functions with a variable number of arguments.
-- Its parameters are function, list of its arguments, its result, and `argC`
-- that constraints all arguments of the function.
class
  (ConstructFunction args r ~ f, DeconstructFunction f ~ '(args, r)) =>
  Function f args r argC
  where
  -- | Create a new function
  --
  -- === __Example usage__
  --
  -- >>> printf :: Function Show f args String => f
  -- >>> printf = createFunction (Proxy :: Proxy Show) (\acc a -> acc <> show a) id ""
  -- >>> printf "hello" () :: String
  -- "hello()"
  createFunction ::
    -- | Required for unambiguous choice of Function instance
    proxy argC ->
    -- | Combine arguments with accumulator
    (forall a. argC a => acc -> a -> acc) ->
    -- | Make result of the function
    (acc -> r) ->
    -- | Accumulator
    acc ->
    f

  -- | Create a function with the same arguments as given one but may have a different result.
  transformFunction ::
    -- | Required for unambiguous choice of the Function instance
    proxy argC ->
    -- | Combine arguments with accumulator
    (forall a. argC a => acc -> a -> acc) ->
    -- | Create result of the `f` function using accumulator and the result of the function to transform
    (acc -> r0 -> r) ->
    -- | Accumulator
    acc ->
    -- | The function to transform
    ConstructFunction args r0 ->
    -- | The new function
    f

-- | Extract list of arguments and the result from the function.
type family DeconstructFunction (f :: Type) :: ([Type], Type) where
  DeconstructFunction (a -> f) = MapFst ((:) a) (DeconstructFunction f)
  DeconstructFunction x = '( '[], x)

type family MapFst (f :: k1 -> k1) (tuple :: (k1, k2)) where
  MapFst f '(a, b) = '(f a, b)

-- | Given the types of function arguments and its result, make a type of a function.
type family ConstructFunction (args :: [Type]) (r :: Type) where
  ConstructFunction '[] r = r
  ConstructFunction (a : args) r = a -> ConstructFunction args r

instance
  ('( '[], r) ~ DeconstructFunction r) =>
  Function r '[] r argC
  where
  createFunction _ _ fr r = fr r
  transformFunction _ _ fr acc r = fr acc r

instance
  (Function f args r argC, argC a) =>
  Function (a -> f) (a : args) r argC
  where
  createFunction pArgC fa fr acc = createFunction pArgC fa fr . fa acc
  transformFunction pArgC fa fr acc f = \a -> transformFunction pArgC fa fr (fa acc a) (f a)

-- | When the arguments are not constrained, use this as the argC parameter of `Function`.
class EmptyConstraint a

instance EmptyConstraint a

-- | Combine constraints. For example, @Function f args x (Show & Num)@.
class (f x, g x) => (&) f g (x :: k)

instance (f x, g x) => (&) f g x
