module Data.Function.Variadic.Utils
  ( composeN,
    constN,
    mappendN,
  )
where

import Data.Function.Variadic

-- | Function composition for an arbitrary number of arguments.
--
-- >>> (show `composeN` \a b c -> (a + b + c :: Int)) 1 2 3
-- "6"
composeN :: (Function f args b EmptyConstraint, Function g args a EmptyConstraint) => (a -> b) -> g -> f
composeN f = transformFunction (undefined :: p EmptyConstraint) const (\_ r -> f r) ()

-- | Constant function for an arbitrary number of arguments.
--
-- @
-- let const2 = constN :: x -> a -> b -> x
-- @
--
-- >>> zipWith3 (constN 1) [1..10] [1..5] ["a", "b", "c"] :: [Int]
-- [1,1,1]
constN :: Function f args a EmptyConstraint => a -> f
constN a = createFunction (undefined :: p EmptyConstraint) const (const a) ()

-- | Append multiple monoid values. It is similar to `mconcat` but takes the values as arguments rather than list elements.
--
-- >>> mappendN [1, 2] [3] [4, 5] :: [Int]
-- [1,2,3,4,5]
mappendN :: forall r f args. (Function f args r ((~) r), Monoid r) => f
mappendN = createFunction (undefined :: proxy ((~) r)) mappend id mempty
