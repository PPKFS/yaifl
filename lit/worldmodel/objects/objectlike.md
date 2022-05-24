## ObjectLike

Something that appears often in the code but isn't really its own thing is `ObjectLike`; something that *can* be evaluated to an `Object` in a context `m`, but isn't necessarily an object itself. `NoMissingObjects` and `MonadWorld` will be explained more in the [Effects](foundations/effects.md) section, but for now they can be read as "block where failed `Entity` lookups are fail conditions" and "block where we can query the existence of an object".

```haskell id=objectlike

class HasID o => ObjectLike wm o where
  getRoom :: (NoMissingObjects m, MonadWorld wm m) => o -> m (Room wm)
  default getRoom :: (NoMissingObjects m) => o -> m (Room wm)
  getRoom o = throwError $ MissingObject "Called getRoom on an object with no instance."  (getID o)

  getThing :: (NoMissingObjects m, MonadWorld wm m) => o -> m (Thing wm)
  default getThing :: (NoMissingObjects m) => o -> m (Thing wm)
  getThing o = throwError $ MissingObject "Called getThing on an object with no instance."  (getID o)

instance ObjectLike wm (Thing wm) where
  getThing = pure

instance ObjectLike wm (Room wm) where
  getRoom = pure

instance ObjectLike wm (AnyObject wm) where
  getThing t = liftEither
    (maybeToRight (MissingObject ("Tried to get a thing from " <> show (_objID t) <> " but it was a room.") (getID t))
      (preview _Thing t))
  getRoom t = liftEither
    (maybeToRight (MissingObject ("Tried to get a room from " <> show (_objID t) <> " but it was a thing.") (getID t))
      (preview _Room t))
```

It's a little annoying because it's basically a "fail but with a nice error message" which isn't ideal.
