# Effects

Extensible (or algebraic) effects are cool, but I don't want to get into depth here and equally I cannot because I am too dumb. The jist of them is instead of a fixed ordering of monads which you can add `MonadFoo` constraints to ala `mtl`:

```haskell
f :: (MonadIO m, MonadBar m) => m a
```

You instead write GADTs with some type wizardry to achieve the same thing as typeclasses. The major advantage is that you can interpret these in multiple different ways (consider a logging effect that can be chosen to be viewed as an IO computation, or a pure ignore effect) and you don't need to deal with the O(n^2) instance problem:

```haskell

data Bar m a where
  Log :: String -> Bar m ()
  SomethingElse :: Bool -> Int -> Bar m a

runBarAsIO :: ...

runBarAsPure :: ...
```

## Why effects and not mtl?

The main reason is because effect frameworks are cool.

## This section

This section just has the two non-specific effects I've written; one for logging and one for printing text to the screen.
