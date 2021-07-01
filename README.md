# variadic-function

Create and transform functions with variable arity.

### How to use

The core of the library is the `Function` class. Use `createFunction` to make a function with a variable number of arguments. Use `transformFunction` to access the arguments and change the result of any function.


```haskell
constN :: Function f args a EmptyConstraint
       => a -> f
constN a = createFunction
  -- The arguments are not constrained
  (undefined :: p EmptyConstraint)
  -- Combine argument with accumulator. Here we just ignore the argument
  const
  -- Ignore the accumulator and return `a` as a result
  (const a)
  -- Accumulator for combining with the arguments.
  -- We don't take any information from the arguments, so it is just ()
  ()

composeN :: (Function f args b EmptyConstraint, Function g args a EmptyConstraint)
         => (a -> b) -> g -> f
composeN f = transformFunction
  -- The arguments are not constrained
  (undefined :: p EmptyConstraint)
  -- Ignore arguments
  const
  -- Ignore the accumulator and apply f to result of the original function `g`
  (\_ r -> f r)
  -- Composition does not use the accumulator either, so it is ()
  ()

Here is a more complex example that constrains arguments and uses the accumulator:

```haskell
sumN :: forall r f args. (Function f args r ((~) r), Num r)
     => f
sumN = createFunction
  -- The argument must be the same type as the function result. 
  -- To be able to mention `r` in here, the function signature 
  -- has `forall` and ScopedTypeVariables is enabled.
  (undefined :: proxy ((~) r))
  -- Add argument to the accumulator
  (+)
  -- Return accumulator as the result
  id
  -- The initial value of accumulator
  0
```