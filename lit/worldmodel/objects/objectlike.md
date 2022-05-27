# ObjectLike and Object querying

`ObjectLike` is something that *can* be evaluated to an `Object` when a `ObjectQuery` effect is present, but isn't necessarily an object itself. Even though for the most part we try to encapsulate object querying into the upper layers of the layer cake and work in pure contexts, it's still useful to not have to manually convert between `ID` and `Object` and `AnyObject`.

```haskell id=objectlike

```

It's a little annoying because it's basically a "fail but with a nice error message" which isn't ideal.
